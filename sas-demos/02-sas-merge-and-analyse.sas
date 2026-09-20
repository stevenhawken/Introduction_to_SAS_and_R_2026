/*==========================================================================
  SESSION 2 · SAS COMPANION — merging, and the bug that ruins analyses
  MSc Epidemiology · Intro to R & SAS

  This is the SAS version of the joins section of Session 2. Run it alongside
  the R code and watch the same trap appear in both languages.

  In SAS the trap is LOUDER — it produces a NOTE in the log — but only if you
  read the log. In R it is completely silent. Neither will stop you.
==========================================================================*/

%LET root = /home/steven.hawken0/seminar;      /* <-- EDIT THIS ONE LINE */

OPTIONS FORMCHAR="|----|+|---+=|-/\<>*" NODATE NONUMBER;
LIBNAME sem "&root";


/*--------------------------------------------------------------------------
  1. IMPORT THE THREE FILES
--------------------------------------------------------------------------*/

%MACRO get(file, out);
  PROC IMPORT DATAFILE="&root/data/&file..csv" OUT=&out DBMS=CSV REPLACE;
    GETNAMES=YES; GUESSINGROWS=MAX;
  RUN;
%MEND get;

%get(births,    births);
%get(mothers,   mothers);
%get(screening, screening);

/* How many rows in each?  1239 / 1200 / 1338 */
PROC SQL;
  SELECT "births"    AS file, COUNT(*) AS n FROM births
  OUTER UNION CORR
  SELECT "mothers",         COUNT(*)        FROM mothers
  OUTER UNION CORR
  SELECT "screening",       COUNT(*)        FROM screening;
QUIT;

/* 1239 infants but only 1338 screening rows for how many infants? */
PROC SQL;
  SELECT COUNT(DISTINCT infant_id) AS n_infants_screened FROM screening;
QUIT;


/*--------------------------------------------------------------------------
  2. TAKE 1 — MERGE WITHOUT A BY STATEMENT
--------------------------------------------------------------------------*/

DATA take1;
  MERGE births screening;
RUN;
/* Read the log and PROC PRINT a few rows. What actually happened?
   Without BY, SAS merges POSITIONALLY — row 1 with row 1, row 2 with row 2.
   The result is nonsense and SAS does not complain. */

PROC PRINT DATA=take1(OBS=5); VAR infant_id specimen_no tsh; RUN;


/*--------------------------------------------------------------------------
  3. TAKE 2 — ADD BY, BUT FORGET TO SORT
--------------------------------------------------------------------------*/

DATA take2;
  MERGE births screening;
  BY infant_id;
RUN;
/* ERROR: BY variables are not properly sorted.
   SAS MERGE requires both datasets sorted by the BY variable(s). Always. */


/*--------------------------------------------------------------------------
  4. TAKE 3 — SORT FIRST. NOW WATCH THE ROW COUNT.
--------------------------------------------------------------------------*/

PROC SORT DATA=births    OUT=b; BY infant_id; RUN;
PROC SORT DATA=screening OUT=s; BY infant_id specimen_no; RUN;

DATA take3;
  MERGE b s;
  BY infant_id;
RUN;

/* ---- READ THE LOG ---------------------------------------------------
   NOTE: MERGE statement has more than one data set with repeats of BY values
   NOTE: The data set WORK.TAKE3 has 1372 observations

   Your cohort started with 1239 infants and now has 1372 rows, because
   133 infants had a repeat screening specimen. Every subsequent mean,
   count and model is now weighted towards infants who were screened twice
   — which is to say, towards the sickest infants.
   --------------------------------------------------------------------- */

PROC SQL;
  SELECT COUNT(*) AS rows, COUNT(DISTINCT infant_id) AS infants FROM take3;
QUIT;


/*--------------------------------------------------------------------------
  5. TAKE 4 — THE FIX: ONE SCREENING RECORD PER INFANT
--------------------------------------------------------------------------*/

DATA s1;
  SET s;
  WHERE specimen_no = 1;       /* keep the initial specimen only */
RUN;

/* IN= flags tell you which source each row came from.
   This is how you write an inner join or a left join in a DATA step. */
DATA cohort;
  MERGE b(IN=inbirth) s1(IN=inscreen);
  BY infant_id;
  IF inbirth;                  /* LEFT JOIN: keep every infant */
  screened = inscreen;         /* 1 = linked to a screening record */
RUN;

/* Should now be 1239 again */
PROC SQL; SELECT COUNT(*) AS n FROM cohort; QUIT;
PROC FREQ DATA=cohort; TABLES screened; RUN;


/*--------------------------------------------------------------------------
  6. WHO DIDN'T LINK, AND ARE THEY DIFFERENT?
     (this is the SAS equivalent of anti_join() — and it belongs in every
      linkage paper you will ever write)
--------------------------------------------------------------------------*/

PROC MEANS DATA=cohort N MEAN STD;
  CLASS screened;
  VAR ga_weeks birth_weight_g;
RUN;
/* If the unlinked infants are systematically earlier or smaller, your
   complete-case analysis is biased and the Limitations section has to say so. */


/*--------------------------------------------------------------------------
  7. ADD THE MOTHERS (many infants to one mother — twins)
--------------------------------------------------------------------------*/

PROC SORT DATA=cohort  OUT=c2; BY momid; RUN;
PROC SORT DATA=mothers OUT=m;  BY momid; RUN;

DATA analysis;
  MERGE c2(IN=inc) m(IN=inm);
  BY momid;
  IF inc;
  sex     = UPCASE(sex);
  preterm = (ga_weeks < 37);
  l_17ohp = LOG(x17ohp);        /* 17-OHP is strongly right-skewed */
RUN;

PROC SQL; SELECT COUNT(*) AS n FROM analysis; QUIT;   /* still 1239 */

/* How many mothers contribute more than one infant? */
PROC SQL;
  SELECT COUNT(*) AS n_mothers_with_twins FROM
    (SELECT momid FROM analysis GROUP BY momid HAVING COUNT(*) > 1);
QUIT;
/* Those infants are NOT independent. A standard PROC LOGISTIC understates
   the standard errors. See PROC GENMOD with REPEATED SUBJECT=momid, or
   PROC GLIMMIX with a random intercept. */


/*--------------------------------------------------------------------------
  8. THE SAME ANALYSIS AS THE R SLIDES
--------------------------------------------------------------------------*/

/* Table 1, by preterm status */
PROC MEANS DATA=analysis N NMISS MEAN STD;
  CLASS preterm;
  VAR mat_age mat_bmi birth_weight_g ga_weeks;
RUN;

PROC FREQ DATA=analysis;
  TABLES preterm*(smoking hypertension diabetes_pre sex) / CHISQ NOCOL NOPERCENT;
RUN;

/* 17-OHP vs gestational age, on the log scale */
PROC SGPLOT DATA=analysis;
  SCATTER X=ga_weeks Y=l_17ohp / TRANSPARENCY=0.6;
  LOESS   X=ga_weeks Y=l_17ohp / NOMARKERS LINEATTRS=(THICKNESS=3);
  YAXIS LABEL="log 17-OHP";
  XAXIS LABEL="Gestational age (weeks)";
  TITLE "17-OHP falls steeply with increasing gestational age";
RUN;
TITLE;

/* Linear model: birth weight */
PROC GLM DATA=analysis;
  CLASS sex smoking;
  MODEL birth_weight_g = ga_weeks sex smoking mat_age / SOLUTION;
RUN; QUIT;

/* Logistic model: preterm birth.
   DESCENDING makes SAS model P(preterm = 1), which is almost certainly what
   you want. Omit it and you silently model P(preterm = 0) and every odds
   ratio is inverted. This is the SAS equivalent of R's reference-level trap. */
PROC LOGISTIC DATA=analysis DESCENDING;
  CLASS smoking (REF='no') / PARAM=REF;
  MODEL preterm = mat_age smoking hypertension diabetes_pre;
RUN;

/* Accounting for clustering within mother */
PROC GENMOD DATA=analysis DESCENDING;
  CLASS momid smoking (REF='no');
  MODEL preterm = mat_age smoking hypertension diabetes_pre / DIST=BINOMIAL LINK=LOGIT;
  REPEATED SUBJECT=momid / TYPE=IND;
RUN;


/*--------------------------------------------------------------------------
  9. EXPORT, SO THE RESULTS LEAVE SAS
--------------------------------------------------------------------------*/

PROC EXPORT DATA=analysis
  OUTFILE="&root/analysis_cohort.csv"
  DBMS=CSV REPLACE;
RUN;

/* ...and R can read the SAS dataset directly, no export needed:
      haven::read_sas("analysis.sas7bdat")
   which is how most people actually move between the two.              */
