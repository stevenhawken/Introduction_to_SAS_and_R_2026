/*==========================================================================
  SESSION 1 · SAS LIVE DEMO — the basics
  MSc Epidemiology · Intro to R & SAS

  Run this in SAS OnDemand for Academics:
    https://www.sas.com/en_ca/software/on-demand-for-academics.html

  SETUP (do this once):
    1. Upload the seminar `data` folder to your SAS OnDemand Files area.
    2. Edit the %LET on the next line so it points at YOUR home folder.
       In OnDemand it looks like  /home/<your-userid>/seminar
==========================================================================*/

%LET root = /home/steven.hawken0/seminar;      /* <-- EDIT THIS ONE LINE */

/* Make output readable as plain text (handy for pasting into notes) */
OPTIONS FORMCHAR="|----|+|---+=|-/\<>*" NODATE NONUMBER;


/*--------------------------------------------------------------------------
  1. THE TWO KINDS OF STEP

     DATA step  -> works one ROW at a time   (create, filter, derive, merge)
     PROC step  -> works on the whole TABLE  (summarise, tabulate, model, plot)

     Every statement ends with a semicolon. Every step ends with RUN;
--------------------------------------------------------------------------*/

/* A DATA step that types the data in directly */
DATA clinic;
  INPUT id site $ ga_weeks bw_g;      /* $ marks a CHARACTER variable */
  bw_kg   = bw_g / 1000;
  preterm = (ga_weeks < 37);          /* a condition IS a 0/1 value in SAS */
  LABEL bw_kg = "Birth weight (kg)";
  DATALINES;
1 Ottawa   38.4 3250
2 Ottawa   41.1 4100
3 Kingston 34.0 2280
4 Sudbury  39.7 3510
5 Kingston 27.3  980
;
RUN;

PROC PRINT DATA=clinic LABEL; RUN;

/* VARNUM lists variables in creation order instead of alphabetically.
   Always use it — alphabetical order tells you nothing about the data. */
PROC CONTENTS DATA=clinic VARNUM; RUN;


/*--------------------------------------------------------------------------
  2. LIBRARIES: WHERE DATASETS LIVE

     WORK      = temporary. Everything in it disappears when SAS closes.
                 A one-level name like `clinic` really means `WORK.clinic`.
     LIBNAME   = points a nickname at a folder, for permanent storage.
--------------------------------------------------------------------------*/

LIBNAME sem "&root";           /* double quotes so &root resolves */

DATA sem.clinic;               /* saved to disk as clinic.sas7bdat */
  SET clinic;
RUN;

/* ---- CHECK THE LOG NOW ----------------------------------------------
   You should see:   NOTE: The data set SEM.CLINIC has 5 observations
   The observation count in the log is the single most useful number
   SAS gives you. Read it after EVERY step.
   ------------------------------------------------------------------- */


/*--------------------------------------------------------------------------
  3. IMPORT A CSV

     Do this once with Tasks and Utilities > Import Data, then copy the
     generated code here so that next time the whole program runs itself.
--------------------------------------------------------------------------*/

PROC IMPORT DATAFILE="&root/data/births.csv"
  OUT=births
  DBMS=CSV
  REPLACE;
  GETNAMES=YES;
  GUESSINGROWS=MAX;     /* read every row before deciding column types */
RUN;

PROC CONTENTS DATA=births VARNUM; RUN;

/* GUESSINGROWS matters: without it SAS guesses types from the first 20 rows
   and will happily truncate a long character value or misread a column that
   turns numeric halfway down the file. This is the SAS version of the same
   trap read_csv() has in R. */


/*--------------------------------------------------------------------------
  4. DESCRIBE AND TABULATE
--------------------------------------------------------------------------*/

PROC FREQ DATA=births;
  TABLES sex site delivery_mode;
RUN;
/* Look at `sex`. Four categories for a binary variable: F, M, f, m.
   Find this now, not in your results section. */

PROC FREQ DATA=births;
  TABLES sex*site / NOPERCENT NOCOL;
RUN;

PROC MEANS DATA=births N NMISS MEAN STD MIN P25 MEDIAN P75 MAX;
  CLASS site;
  VAR ga_weeks birth_weight_g;
RUN;

PROC UNIVARIATE DATA=births;
  VAR birth_weight_g;
  HISTOGRAM birth_weight_g / NORMAL;
RUN;


/*--------------------------------------------------------------------------
  5. DERIVE VARIABLES IN A DATA STEP
--------------------------------------------------------------------------*/

DATA births2;
  SET births;

  /* harmonise the messy sex coding */
  sex = UPCASE(sex);

  bw_kg   = birth_weight_g / 1000;
  preterm = (ga_weeks < 37);
  lbw     = (birth_weight_g < 2500);

  /* IF / ELSE IF stops at the first true condition — same logic as
     case_when() in R. Order from most specific to least. */
  IF      ga_weeks < 32 THEN ga_group = "very preterm";
  ELSE IF ga_weeks < 37 THEN ga_group = "preterm     ";
  ELSE IF ga_weeks < 42 THEN ga_group = "term        ";
  ELSE                       ga_group = "post-term   ";

  /* parse the birth datetime and keep a plain date */
  birth_dt = INPUT(birth_datetime, ANYDTDTM.);
  dob      = DATEPART(birth_dt);
  FORMAT birth_dt datetime16. dob date9.;
RUN;

PROC FREQ DATA=births2; TABLES ga_group preterm lbw sex; RUN;

/* NOTE on character length: `ga_group` takes its length from the FIRST
   assignment SAS sees. That is why the strings above are padded to equal
   width. Forget this and "very preterm" silently becomes "very". It is an
   ugly quirk and it catches everyone at least once.
   The clean fix is to declare it first:  LENGTH ga_group $12;            */


/*--------------------------------------------------------------------------
  6. PLOTS
--------------------------------------------------------------------------*/

PROC SGPLOT DATA=births2;
  HISTOGRAM ga_weeks / BINWIDTH=0.5;
  REFLINE 37 / AXIS=X LINEATTRS=(PATTERN=DASH THICKNESS=2);
  XAXIS LABEL="Gestational age (completed weeks)";
  TITLE "Gestational age at birth";
RUN;

PROC SGPLOT DATA=births2;
  VBOX birth_weight_g / CATEGORY=site GROUP=sex;
  YAXIS LABEL="Birth weight (g)";
  TITLE "Birth weight by site and sex";
RUN;

PROC SGPLOT DATA=births2;
  SCATTER X=ga_weeks Y=birth_weight_g / GROUP=sex TRANSPARENCY=0.6;
  REG     X=ga_weeks Y=birth_weight_g / NOMARKERS;
  TITLE "Birth weight vs gestational age";
RUN;
TITLE;


/*--------------------------------------------------------------------------
  7. THE THREE WORDS TO SEARCH FOR IN THE LOG

     ERROR    — it stopped. You will notice.
     WARNING  — it kept going. You might not notice.
     NOTE     — the dangerous one. Read these:

       NOTE: Invalid numeric data, 'N/A' , at line ...
       NOTE: Missing values were generated as a result of ...
       NOTE: MERGE statement has more than one data set with repeats of BY values
       NOTE: The data set WORK.X has <N> observations   <-- check N every time

     Deliberately break something and read the log:
--------------------------------------------------------------------------*/

DATA oops;
  SET births;
  silly = birth_weight_g / 0;    /* division by zero */
  bad   = ga_weeks + site;       /* number + character */
RUN;
/* Now read the log. SAS produced a dataset anyway. It always does. */
