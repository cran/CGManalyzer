/*
 -------------------------------------------------------------------------------
 C codes for calculating multiscale entropy (MSE) of one or multiple data sets

 Compile this program by
 gcc -o mseLong -O mseLong.c -lm -w
 The codes essentially come from http://www.physionet.org/
 -------------------------------------------------------------------------------
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <string.h>

#define MAXSTR 1250     /* maximum string size */
#define DATA_MAX 600000 /* maximum number of data points */
#define M_MAX 10        /* maximum value of the parameter m */
#define SCALE_MAX 100    /* maximum scale value */
#define R_STEP_MAX 10   /* maximum number of different r values */
#define FILEMAX 100     /* maximum number of data files in the list file*/

/* Global variables */
char *prog, line[MAXSTR];
double SE[FILEMAX][R_STEP_MAX][SCALE_MAX][M_MAX];
double *u, *y, r_min, r_max, r_step;
int c, nlin, m_min, m_max, m_step, scale_max, scale_step, i_min, i_max;

static char file[500];

FILE *fl;
FILE *pin;
FILE *pout;

void Mse(char** cmdstrp);
void MseDo(int argc, char* argv[]);

//int main(int argc, char* argv[])
//{
//
//    char cmdstr[] = "MSE.exe -n 10 -a 1 -b 1 -m 2 -M 2 -r 0.15 -R 0.15 -I 4000 -z temp.txt -Z result.txt";
//    char **p = &cmdstr;
//    printf("%s\n", *p);
//    return 0;
//}

void Mse(char** cmdstrp)
{
  char cmdstr[1024];
  strcpy(cmdstr,*cmdstrp);
  int argc = 0;
  char *argv[30];
  const char *delim = " ";
  char *p = strtok(cmdstr, delim);

  do
  {
    argv[argc++] = p;
    p = strtok(NULL, delim);
  }while (p != NULL);

  MseDo(argc,argv);
}

void MseDo(int argc, char* argv[])
{

  int i, j, l, nfile, flag;
  double r, sd, tmp;

  /* Function prototypes */
  void ReadData(void);
  double *array(int n);
  double StandardDeviation(void);
  void CoarseGraining (int j);
  void SampleEntropy (int ll, double r, double sd, int j);
  void PrintResults (int nfile);
  void PrintAverageResults (int nfile);

  /* Initialize variables. */
  prog = argv[0];
  scale_max = 20;
  scale_step = 1;
  m_min = 2;
  m_max = 2;
  m_step = 1;
  i_min = 0;
  i_max = 39999;
  r_min = 0.15;
  r_max = 0.15;
  r_step = 0.05;
  c = 0;
  nfile = 0;
  flag = 0;

  /* Read and interpret the command line. */
  i = 0;
  while (++i < argc && *argv[i] == '-') {
    switch(argv[i][1]) {

    case 'F':
      if ((fl = fopen(argv[++i], "r")) == NULL) {
        error("%s [-F]: can't open input file %s\n",
                prog, argv[i]);
      }
      flag=1;
      break;

    case 'n':
      if ((scale_max=atoi(argv[++i])) <= 0 || scale_max > SCALE_MAX) {
        error("%s [-n]: maximum scale must be between 1 and %d (default: 20)\n",
                prog, SCALE_MAX);
      }
      break;

    case 'a':
      if ((scale_step=atoi(argv[++i])) <= 0 || scale_step >= SCALE_MAX) {
        error("%s [-a]: scale increment must be between 1 and %d (default: 1)\n",
                prog, SCALE_MAX);
      }
      break;

    case 'm':
      if ((m_min = atoi(argv[++i])) >= M_MAX || m_min <= 0 ) {
        error("%s [-m]: minimum m value must be between 1 and %d (default: 2)\n",
                prog, M_MAX);
      }
      break;

    case 'M':
      if ((m_max = atoi(argv[++i])) >= M_MAX || m_max <= 0) {
        error("%s [-M]: maximum m value must be between 1 and %d (default: 2)\n",
                prog, M_MAX);
      }
      break;

    case 'b':
      if ((m_step = atoi(argv[++i])) <= 0 || m_step > M_MAX) {
        error("%s [-b]: m increment must be between 1 and %d (default: 1)\n",
                prog, M_MAX);
      }
      break;

    case 'r':
      if ((r_min = atof(argv[++i])) <= 0) {
        error("%s [-r]: minimum r must be greater than 0 (default: 0.15)\n",
                prog);
      }
      break;

    case 'R':
      if ((r_max = atof(argv[++i])) <= 0) {
        error("%s [-R]: maximum r must be greater than 0 (default: 0.15)\n",
                prog);
      }
      break;

    case 'c':
      if ((r_step=atof(argv[++i]))<=0||r_step<(r_max-r_min)/R_STEP_MAX) {
        error("%s [-c]: r increment must be greater than %g (default: 0.05)\n",
                prog, (r_max-r_min)/R_STEP_MAX);
      }
      break;

    case 'i':
      if ((i_min = atoi(argv[++i])) < 0) {
        error("%s [-i]: minimum i must not be less than 0 (default: 0)\n",
                prog);
      }
      break;

    case 'I':
      if ((i_max = atoi(argv[++i])) <= 0 || i_max <= i_min) {
        error("%s [-I]: maximum i must be greater than %d "
                "(default: number of data points)\n",
                prog, i_min);
      }
      break;
    case 'z':
      if ((pin = fopen(argv[++i], "r")) == NULL) {
        error("%s [-z]: can't open input file %s\n",
                prog, argv[i]);
      }
      break;
    case 'Z':
      if ((pout = fopen(argv[++i], "w")) == NULL) {
        error("%s [-Z]: can't open output file %s\n",
                prog, argv[i]);
      }
      break;
    default:
      error("Unsupported parameters are passed when call C function Mse");
    }
  }

  if (m_max < m_min) {
    tmp = m_max;
    m_max = m_min;
    m_min = tmp;
  }

  if (r_max < r_min){
    tmp = r_max;
    r_max = r_min;
    r_min = tmp;
  }

  /* Memory allocation. */
  u = array(DATA_MAX);
  y = array(DATA_MAX);


  /* Process a single data file. */
  if (flag == 0) {
    l = 0;
    nfile = 1;

    /* Read data from stdin. */
    //	pin = stdin;
    ReadData();

    /* Calculate standard deviation. */
    sd = StandardDeviation();

    /* Perform the coarse-graining procedure. */
    for (j = 1; j <= scale_max; j += scale_step){
      CoarseGraining(j);

      /* Calculate SampEn for each scale and each r value. */
      c = 0;
      for (r = r_min; r <= (r_max*1.0000000001); r += r_step){
        SampleEntropy(l, r, sd, j);
        c++;
      }
    }

    /* Print results. */
    PrintResults(nfile);

    fclose(pin);
    fclose(pout);
  }

  free(u);
  free(y);
}

double *array(int n)
{
  double *a;

  if ((a = calloc (n, sizeof (double))) == NULL) {
    error("%s : insufficient memory\n", prog);
  }
  return (a);
}

void ReadData(void)
{
  int j = -1;

  while (fgets(line, MAXSTR, pin) != NULL) {
    j++;
    if (j >= i_min && j <= i_max) {
      sscanf(line, "%lf", &u[j-i_min]);
      nlin=j-i_min+1;
    }
  }
}

double StandardDeviation(void)
{
  double sum=0.0, sum2=0.0, sd;
  int j;

  for (j = 0; j < nlin; j++) {
    sum += u[j];
    sum2 += u[j] * u[j];
  }
  sd = sqrt((sum2 - sum*sum/nlin)/(nlin - 1));
  return (sd);
}


void CoarseGraining(int j)
{
  int i, k;

  for (i = 0; i < nlin/j; i++) {
    y[i] = 0;
    for (k = 0; k < j; k++)
      y[i] += u[i*j+k];
    y[i] /= j;
  }
}


void SampleEntropy(int ll, double r, double sd, int j)
{
  int i, k, l, nlin_j;
  long long cont[M_MAX+1];
  double r_new;

  nlin_j = (nlin/j) - m_max;
  r_new = r*sd;

  for (i = 0; i < M_MAX; i++)
    cont[i]=0;

  for (i = 0; i < nlin_j; ++i) {
    for (l = i+1; l < nlin_j; ++l) { /*self-matches are not counted*/
    k = 0;
      while (k < m_max && fabs(y[i+k] - y[l+k]) <= r_new)
        cont[++k]++;
      if (k == m_max && fabs(y[i+m_max] - y[l+m_max]) <= r_new)
        cont[m_max+1]++;
    }
  }

  for (i = 1; i <= m_max; i++)
    if (cont[i+1] == 0 || cont[i] == 0)
      SE[ll][c][j][i] = -log((double)1/((nlin_j)*(nlin_j-1)));
    else
      SE[ll][c][j][i] = -log((double)cont[i+1]/cont[i]);
}


void PrintResults(int nfile)
{
  int j, m, k, l;

  fprintf (pout, "\n");
  fprintf (pout, "\nmax_line_read = %12d", nlin);
  fprintf (pout, "\n");

  for (m = m_min; m <= m_max; m += m_step)
    for (k = 0; k < c; k++) {
      fprintf (pout, "\nm = %d,  r = %.3f\n\n", m, r_min+k*r_step);
      if (nfile > 1) {
        fseek(fl, 0, SEEK_SET);
        for(l = 0; fscanf(fl, "%s", file) == 1; l++)
          fprintf (pout, "\t%.6s", file);
        fprintf (pout, "\n");
      }
      for (j = 1; j <= scale_max; j += scale_step) {
        fprintf (pout, "%d\t", j);
        for (l=0; l<nfile; l++)
        {
          fprintf (pout, "%.3lf\t", SE[l][k][j][m]);
        }
        fprintf (pout, "\n");
      }
    }
}

void PrintAverageResults(int nfile)
{
  int k, m, j, i, l;
  double av, av2, sd1;

  fprintf (pout, "\n**************************\n");
  fprintf (pout, "Mean and SD over all files\n");
  fprintf (pout, "**************************\n");

  for (k = 0; k < c; k++) {
    fprintf (pout, "\n");

    for (m = m_min; m <= m_max; m += m_step)
      fprintf (pout, "\tm=%d, r=%5.3lf", m, r_min+k*r_step);
    fprintf (pout, "\n");
    for (m = m_min; m <= m_max; m += m_step)
      fprintf (pout, "\tmean\tsd");
    fprintf (pout, "\n");

    for (j = 1; j <= scale_max; j += scale_step) {
      fprintf (pout, "%d\t", j);
      for (i = m_min; i <= m_max; i += m_step) {
        av = 0.0;
        av2 = 0.0;
        /* Calculate entropy mean values and SD over all files. */
        for (l = 0; l < nfile; l++) {
          av += SE[l][k][j][i];
          av2 += (SE[l][k][j][i]*SE[l][k][j][i]);
        }
        sd1 = sqrt((av2-av*av/nfile) / (nfile-1));
        av /= nfile;
        fprintf (pout, "%.3lf\t%.3lf\t", av, sd1);
      }
      fprintf (pout, "\n");
    }
  }
}

