/* 
   Skeleton "interpreter" for numeric processing on CSV files
 
   Written by Alistair Moffat, May 2015

   Modifications by Tessa Song, May 2015
   
   Attribution: 
     bigger_double function in this script were refered from
     http://people.eng.unimelb.edu.au/ammoffat/teaching/20005/class-ex8.c
     written by Alistair Moffat
   
   Login name : songt
   Student ID : 597952
   
   programming is fun
   
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>
#include <assert.h>

#define MAXROWS 1000
#define MAXCOLS 50

#define LINELEN 500 /* maximum length of any input line */

#define ERROR   (-1)    /* error return value from some functions */
#define COMMA   ',' /* separator for CSV files */

#define INDEXX  'i' /* command to list the column headings */
#define DATDMP  'd' /* command to dump out the data */
#define AVERGE  'a' /* command to average a column */
#define CATAVG  'c' /* command to category-average a column */
#define GRAPH1  'g' /* command to graph a column */
#define GRAPH2  'p' /* command to plot a 2d correlation */
#define KNDALL  'k' /* command to compute Kendall's tau */
#define ALLOPS  "acdgikp"
            /* list of all valid commands */

#define ARGS_0  "di"    /* commands that take no arguments */
#define ARGS_1  "ag"    /* commands that take one argument */
#define ARGS_2  "ckp"   /* commands that take two arguments */
#define MAXARGS  2      /* maximum number of arguments to any command */

#define GRAPHROWS 20    /* number of rows in graph */
#define GRAPHCOLS 60    /* number of columns in graph */
#define EPSILON   1e-6  /* small adjustment for graph computation */

#define MAXCATS 1000    /* maximum number of categories to be handled */

#define FILINP  1   /* indicates command input coming from a file */
#define PROMPT  "> "

#define EQUAL 0    /* indicates two given float numbers are neraly equal */
#define SMALLER -1 /* indicates the former number is smaller */
#define BIGGER 1   /* indicates the former number is bigger */

/****************************************************************/

/* structure declarations -- probably no need to change these,
   but you can if you wish... */

typedef char input_line_t[LINELEN+1];

typedef struct {
    input_line_t labelstring;
    char *labs[MAXCOLS+1];
    int nrows;
    int ncols;
    double vals[MAXROWS][MAXCOLS];
} csv_t;

typedef struct {
    char command;
    int nargs;
    int argvals[MAXARGS];
} command_t;


typedef struct {
    double value; /* category value */
    double sum;   /* sum of all the elements in this category */
    int num;      /* number of elements which falls in this category*/
} category_t;


typedef struct {
    double vals[MAXROWS]; /* array of numbers this bucket has */
    int len;              /* length of the array */
} bucket_t;


typedef struct {
    double min; /* mininum value */
    double max; /* maximum value */
    double interval; /* the interval between two numbers */
} mms_t;

/****************************************************************/

/* function prototypes */

void    read_csv_file(char *fname, csv_t *D);
void    reassign_input(char *fname);
void    print_prompt();
int     read_command(command_t *comd, int fileinput, int ncols);
void    process_line(command_t *comd, csv_t *D);
void    do_datdmp(csv_t *D);
void    do_indexx(csv_t *D);
void    do_averge(csv_t *D, int col);
void    do_graph1(csv_t *D, int col);
void    do_catavg(csv_t *D, int cat, int col);
void    do_kndall(csv_t *D, int col1, int col2);
void    do_graph2(csv_t *D, int col1, int col2);
void    sort_category(category_t* ctgr_arr, int len);
void    swap(category_t* c1, category_t* c2);
int     bigger_double(double num1, double num2);
void    calculate_mms(csv_t *D, int col, int num_secs, mms_t *mms);
void    initialise_zero(int* arr, int len);

/****************************************************************/

/* main program controls all the action
 */
int
main(int argc, char *argv[]) {
    int fileinput=0;
    command_t comd;
    csv_t D;
    
    /* first argument on commandline is the data file name */
    read_csv_file(argv[1], &D);

    
    /* second argument, if it exists, is file of input commands */
    if (argc==3) {
        fileinput = 1;
        reassign_input(argv[2]);
    }

    /* start the main execution loop */
    print_prompt();
    while (read_command(&comd, fileinput, D.ncols)) {
        process_line(&comd, &D);
        /* then round we go */
        print_prompt();
    }

    /* all done, so pack up and go home */
    printf("bye\n");
    return 0;
}

/****************************************************************/

/* reads a csv file in to the defined structure, with empty or non-numeric
   values replaced by 0.0/0.0 = nan so as to prevent downstream arithmetic
   being interpreted incorrectly. Probably best to just leave this function
   alone, you don't need to edit it to complete the project
*/

void
read_csv_file(char *fname, csv_t *D) {
    FILE *fp;   /* used to read from a named file */
    input_line_t line;
    int cols=0, rows=0, bytes=0;
    int c, i, j, chr, ncommas, empties=0;
    double x;
    double nan = 0.0/0.0;

    /* first argument on commandline should the data file name */
    if (fname==NULL) {
        /* and it wasn't there... */
        printf("No csv file specified on commandline\n");
        exit(EXIT_FAILURE);
    }

    /* try and open the named file for reading */
    if ((fp=fopen(fname,"r")) == NULL) {
        printf("Error: unable to open %s\n", fname);
        exit(EXIT_FAILURE);
    }

    /* file is open, can now use fp to access CSV data,
       start by reading the bytes of the header row */
    while (((c=getc(fp))!=EOF) && (c!='\n' && c!='\r')) {
        D->labelstring[bytes++] = c;
    }
    D->labelstring[bytes] = '\0';

    /* now process line again, breaking in to separate labels by
       replacing commas by nulls, and tracking the start of each of
       the column headings */
    D->labs[cols++] = D->labelstring;
    for (i=1; i<bytes; i++) {
        if (D->labelstring[i]==COMMA) {
            D->labelstring[i] = '\0';
            D->labs[cols++] = D->labelstring+i+1;
        }
        if (cols>MAXCOLS && i<bytes) {
            printf("Too many columns, limit is %d\n",
                MAXCOLS);
            exit(EXIT_FAILURE);
        }
    }
    D->labs[cols] = NULL;

    /* ok, that's the labels sorted, now for the data */
    while ((chr=getc(fp)) != EOF) {

        /* there is another row, because first character of it
           just got read, next step is to get the rest of them */
        i = 0;
        line[i++] = chr;
        ncommas = (chr==COMMA) ;
        while (((chr=getc(fp))!=EOF) && (chr!='\n' && chr!='\r')) {
            line[i++] = chr;
            ncommas += (chr==COMMA) ;
        }
        line[i] = '\0';
        if (ncommas!=cols-1) {
            printf("Data input error line %d\n", rows+2);
            exit(EXIT_FAILURE);
        }
        /* then process the line from the right end */
        j = i-1;
        for (c=cols-1; c>=0; c--) {
            /* look for next previous comma */
            while (j>=0 && line[j]!=COMMA) {
                j--;
            }
            /* access the value */
            if (sscanf(line+j+1, "%lf", &x) == 1) {
                D->vals[rows][c] = x;
            } else {
                D->vals[rows][c] = nan;
                empties++;
            }
            /* mark the new end of the string */
            if (j>=0) {
                line[j--] = '\0';
            }
        }
        rows++;
        /* check to make sure don't overflow array */
        if (rows==MAXROWS) {
            /* time to stop reading data */
            printf("Too many rows, truncated at %d\n", MAXROWS);
            break;
        }
        /* if not full, go round and see if there is another data row */
    }

    /* either input has all been read or array is full */
    printf("file %s:\n    %d columns and %d rows of data\n",
            fname, cols, rows);
    if (empties) {
        printf("    %d entries were empty or non-numeric\n",
            empties);
    }
    /* finish building the structure */
    D->nrows = rows;
    D->ncols = cols;
    return;
}

/****************************************************************/

/* if there is a valid filename on the commandline, redirect stdin
   so that the file is read, and return FILINP to show that input
   input lines should be echoed to the output when they are read
*/
void
reassign_input(char *fname) {
    if (freopen(fname, "r", stdin)==NULL) {
        printf("Unable to open \"%s\"\n", fname);
        exit(EXIT_FAILURE);
    }
    /* stdin successfully reopened to the named file */
    printf("Input file: %s\n", fname);
    return;
}

/****************************************************************/

/* print the "ready for input" prompt
 */
void
print_prompt() {
    printf(PROMPT);
    return;
}

/****************************************************************/

/* read a line of input into the array passed as argument
   returns false if there is no input available
   all whitespace characters are removed
*/
int    
read_command(command_t *comd, int fileinput, int ncols) {
    int i=0, c;
    int col;
    input_line_t line;
    /* get a whole input line, single blank of multiples */
    while (((c=getchar())!=EOF) && (c!='\n')) {
        if (i<LINELEN) {
            line[i] = c;
            if (i==0 || (isspace(line[i-1])*isspace(line[i])==0)) {
                i++;
            }
        }
    }
    line[i] = '\0';
    if (fileinput) {
        /* print out the input command */
        printf("%s\n", line);
    }
    /* nothing typed? straight back to caller */
    if (i==0 && c==EOF) {
        return 0;
    }
    if (i==0) {
        return 1;
    }
    /* something typed? parse into parts needed */
    comd->command = line[0];
    comd->nargs = 0;
    for (i=1; line[i]; i++) {
        if (!isdigit(line[i]) && !isspace(line[i])) {
            printf("Invalid input character\n");
            return 1;
        }
        if (line[i-1]==' ' && line[i]!=' ') {
            col = atoi(line+i);
            comd->argvals[comd->nargs++] = col;
        }
    }
    return ((i>0) || (c!=EOF));
}

/****************************************************************/

/* process a command by parsing the input line into parts and
   carrying out the specified action
 */
void
process_line(command_t *comd, csv_t *D) {
    int optype, col1=0, col2=0;

    /* determine the operation to be performed, it
       must be first character in line
     */
    optype = comd->command;
    if (strchr(ALLOPS, optype) == NULL) {
        printf("Unknown operator\n");
        return;
    }

    /* determine the string argument (if one is required),
       it must start in second character of line
     */
    if (strchr(ARGS_0, optype)) {
        if (comd->nargs!=0) {
            printf("No argument required for '%c'\n", optype);
            return;
        }
    } else if (strchr(ARGS_1, optype)) {
        if (comd->nargs!=1) {
            printf("One argument required for '%c'\n", optype);
            return;
        }
        col1 = comd->argvals[0];
        if (col1>D->ncols) {
            printf("Invalid column number, ");
            printf("max is %d\n", D->ncols);
            return;
        }
    } else if (strchr(ARGS_2, optype)) {
        if (comd->nargs!=2) {
            printf("Two arguments required for '%c'\n", optype);
            return;
        }
        col1 = comd->argvals[0];
        col2 = comd->argvals[1];
        if (col1>D->ncols || col2>D->ncols) {
            printf("Invalid column number, ");
            printf("max is %d\n", D->ncols);
            return;
        }
    }

    /* finally, do the actual operation
     */
    if (optype == INDEXX) {
        do_indexx(D);
    } else if (optype == DATDMP) {
        do_datdmp(D);
    } else if (optype == AVERGE) {
        do_averge(D, col1);
    } else if (optype == GRAPH1) {
        do_graph1(D, col1);
    } else if (optype == CATAVG) {
        do_catavg(D, col1, col2);
    } else if (optype == KNDALL) {
        do_kndall(D, col1, col2);
    } else if (optype == GRAPH2) {
        do_graph2(D, col1, col2);
    }
    return;
}

/****************************************************************/

/* provide an index list of the column headings
*/
void
do_indexx(csv_t *D) {
    int c;
    printf("      col  data\n");
    for (c=0; c<D->ncols; c++) {
        printf("      %3d  %s\n", c+1, D->labs[c]);
    }
    return;
}

/****************************************************************/

/* dump out the data in the CSV structure D
*/
void
do_datdmp(csv_t *D) {
    int r, c;
    /* first the header labels */
    printf("      ");
    for (c=0; c<D->ncols; c++) {
        printf("%10s ", D->labs[c]);
    }
    printf("\n");
    /* now the values in the data rows */
    for (r=0; r<D->nrows; r++) {
        printf("%4d: ", r+1);
        for (c=0; c<D->ncols; c++) {
            printf("%10.2f ", D->vals[r][c]);
        }
        printf("\n");
    }
    return;
}

/****************************************************************/

/* implement the 'a' averaging command
*/
void
do_averge(csv_t *D, int col) {
    double sum = 0.0, avrg;
    int i;
    
    /* iterate all the values in that colum and add them up */
    for(i = 0; i<D->nrows; i++){
        sum += D-> vals[i][col-1];
    }

    /* calculate the average */ 
    avrg = sum/(D->nrows);
    
    printf("average %s: %.2f ", D->labs[col-1], avrg);
    
    printf("(over %d values)\n", D->nrows);

    return;
}

/****************************************************************/

/* implement the 'g' graphing command
*/
void
do_graph1(csv_t *D, int col) {
    int i, j;
    int sc_factor;
    mms_t mms;
    /* declare array for counting elements in each bucket*/
    int buckets[GRAPHROWS] = {0,};
    int bgst_bucket = 0;
    double left, right;
    
    
    /* calculate min, max and interval */    
    calculate_mms(D, col, GRAPHROWS, &mms);
    

    /* fill up the buckets array */
    left = mms.min;
    right = left + mms.interval;
    
    for(i = 0;i<GRAPHROWS; i++){
        for(j = 0; j < D->nrows; j++){
            /* if the value is in interval, increase the number by 1 */
            if( left <= D->vals[j][col-1] && D->vals[j][col-1] < right){
                buckets[i]++;
            }
        }

        /* find the biggest bucket */
        if(bgst_bucket < buckets[i])
            bgst_bucket = buckets[i];
        
        left = right;
        right += mms.interval;
    }
    
    
    /* find scale factor */
    sc_factor = 1;
    while(ceil((double)bgst_bucket/sc_factor) > GRAPHCOLS){
        sc_factor*=2;
    }
    
    
    /* print the graph in reverse order*/
    printf("graph of %s, scaled by a factor of %d\n", 
        D->labs[col-1], sc_factor);
    
    right = mms.max;
    left = right - mms.interval;
    
    for(i = GRAPHROWS-1; i>=0;i--){
        printf("%6.2f--%6.2f [%3d]:", left, right, buckets[i]);
        
        for(j = 0;j < ceil((double)buckets[i]/sc_factor);j++){
            printf("*");
        }
        printf("\n");
        
        right = left;
        left -= mms.interval;
    } 

}

/****************************************************************/

/* implement the 'c' category average command
*/
void
do_catavg(csv_t *D, int cat, int col) {
    
    /* declare array of category_t structs */
    category_t ctgr_arr[MAXCATS] = {{0,0,0}, };
    int arr_len = 0;
    int i,j;
    double val1, val2;
    category_t temp;
    

    ctgr_arr[arr_len++].value = D->vals[0][cat-1];
    
    /* fill up the ctgr_arr */
    for(i = 0; i<D->nrows; i++){
        for(j = 0; j<arr_len; j++){
            val1 = D->vals[i][cat-1];
            val2 = ctgr_arr[j].value;
            
            /* if these two values are the same, 
                          update the jth ctgr sturct */
            if(bigger_double(val1, val2)==0){
                ctgr_arr[j].sum += D->vals[i][col-1];
                ctgr_arr[j].num++;
                break;
            }
        }
        
        /* if there is no such category value, 
                          put the value in the ctgr_array */
        if(j == arr_len){
            ctgr_arr[arr_len].value = D->vals[i][cat-1];
            ctgr_arr[arr_len].sum+=D->vals[i][col-1];
            ctgr_arr[arr_len].num++;
            arr_len++;
        }
    }

    /* sort the array in ascending order */
    sort_category(ctgr_arr, arr_len);
    
    
    printf("%10s   average %s\n", D->labs[cat-1], D->labs[col-1]);

    for(i = 0; i<arr_len; i++){
        temp = ctgr_arr[i];
        printf("%10.2f %10.2f (over %d values)\n",
            temp.value, temp.sum/temp.num, temp.num);
    }
    
    return;
}

/****************************************************************/

/* implement the 'k' command to compute Kendall's tau-b
   coefficient for similarity between two sets of paired values
*/
void   
do_kndall(csv_t *D, int col1, int col2) {
    int concord = 0, discord = 0;
    double coeff;
    int i, j;
    int flag1, flag2;
    double val1, val2, val3, val4;
    
    /* iterate all the possible pairs */
    for(i = 0; i<D->nrows; i++){
        for(j = i+1; j<D->nrows; j++){
            
            val1 = D->vals[i][col1-1];
            val2 = D->vals[j][col1-1];
            
            val3 = D->vals[i][col2-1];
            val4 = D->vals[j][col2-1];
            
            /* compare values */
            flag1 = bigger_double(val1, val2);
            flag2 = bigger_double(val3, val4);
            
            /* if either of the two columns equal to each other */
            if( !flag1 || !flag2 ){
                /* do nothing*/
            }else if( flag1 == flag2 ){
                concord++;
            }else{
                discord++;
            }
        }
    }

    /* calculate correlation coefficient */
    coeff = 2.0*(concord-discord)/(D->nrows*(D->nrows-1));

    printf("tau coefficient between %s and %s = %2.2f\n",
        D->labs[col1-1], D->labs[col2-1], coeff);
    
    return;
}

/****************************************************************/

/* implement the 'p' plot command to generate
   a 2d graph showing correlation between two columns
*/
void   
do_graph2(csv_t *D, int col1, int col2) {
    int i, j, k, l;
    bucket_t bucket_arr[GRAPHROWS];
    int temp_arr[GRAPHCOLS] = {0,};
    
    mms_t mms_c1, mms_c2;
    double left_c1, right_c1, left_c2, right_c2;
    double temp;
    
    /* initialise bucket_arr */
    for(i = 0;i<GRAPHROWS;i++){
        bucket_arr[i].len = 0;
    }
    
    /* calculate the two mms */
    calculate_mms(D, col1, GRAPHROWS, &mms_c1);
    calculate_mms(D, col2, GRAPHCOLS, &mms_c2);

    
    /* fill up the bucket_arr */
    left_c1 = mms_c1.min;
    right_c1 = left_c1 + mms_c1.interval;
    
    
    for(i = 0; i<GRAPHROWS; i++){
        for(j = 0; j<D->nrows; j++){
            temp = D->vals[j][col1-1];
            if(left_c1<=temp && temp<right_c1){
                /* add the number in col2 to 'i'th bucket_t */
                bucket_arr[i].vals[bucket_arr[i].len++]
                                            = D->vals[j][col2-1];
            }
        }
        left_c1 = right_c1;
        right_c1 += mms_c1.interval;
    }

    
    /* print the graph out in reverse order */
    printf("plot of %s (vertical) and %s (horizontal)\n", D->labs[col1-1],
        D->labs[col2-1]);
    
    right_c1 = mms_c1.max;
    left_c1 = right_c1-mms_c1.interval;
    
    for(i = GRAPHROWS-1; i>=0; i--){
        /* fill out the temporary array for this row */
        left_c2 = mms_c2.min;
        right_c2 = left_c2 + mms_c2.interval;
        
        for(j = 0; j<GRAPHCOLS; j++){
            for(k = 0; k<bucket_arr[i].len; k++){
                temp = bucket_arr[i].vals[k];
                
                /* if temp falls in this interval, increase by 1 */
                if( left_c2 <= temp && temp < right_c2 ){
                    temp_arr[j]++;
                }
                
            }
            left_c2 = right_c2;
            right_c2 += mms_c2.interval;
        }
        
        /* print this row */
        printf("%6.2f--%6.2f: ", left_c1, right_c1);
        for(l = 0; l<GRAPHCOLS; l++){
            if(temp_arr[l] == 0){
                printf(".");
            }else{
                printf("%d", (int)log2(temp_arr[l]+1));
            }
        }
        printf("\n");
        
        /* initialise the temporary array with zeros */
        initialise_zero(temp_arr, GRAPHCOLS);
        
        right_c1 = left_c1;
        left_c1 -= mms_c1.interval;
        
    }
    
    return;
}

/****************************************************************/

/* sort array of category_t structs on the basis of 'value'
   in ascending order
*/
void
sort_category(category_t* ctgr_arr, int len){
    
    int i, j;
    for(i = 1; i<len; i++){
        for(j = i-1; j>=0 && 
            ctgr_arr[j].value > ctgr_arr[j+1].value; j--){
        
            /* if the former one is bigger, then swap them */
            swap(&ctgr_arr[j], &ctgr_arr[j+1]);
        }
    }
    
}

/****************************************************************/

/* swap the two category_t structs given
*/
void
swap(category_t* c1, category_t* c2){
    category_t temp;
    temp = *c2;
    *c2 = *c1;
    *c1 = temp;
}

/****************************************************************/

/* returns 0 if the two numbers are nearly equal, -1 if the former one
   is smaller and +1 if the former one is bigger
*/
int bigger_double(double num1, double num2){

    double diff = num1-num2;
    diff = diff/num2;
    
    if (fabs(diff) < EPSILON) {
        /* nearly equal */
        return EQUAL;
    } else if (diff<0) {
        return SMALLER;
    } else {
        return BIGGER;
    }
    
    /* to keep the compiler silent */
    return 0;
}

/****************************************************************/

/* calculate min, max and interval
*/
void 
calculate_mms(csv_t *D, int col, int num_secs, mms_t *mms){
    
    int i;
    
    mms->min = D->vals[0][col-1];
    mms->max = D->vals[0][col-1];
    
    /* find out min and max values */
    for(i = 1; i<D->nrows; i++){
        if(D->vals[i][col-1] < mms->min){
            mms->min = D->vals[i][col-1];
        }
        if(D->vals[i][col-1] > mms->max){
            mms->max = D->vals[i][col-1];
        }
    }
    

    /* adjust the range a bit */
    mms->min -= EPSILON;
    mms->max += EPSILON;
    mms->interval = (mms->max - mms->min)/num_secs;
    
}

/****************************************************************/

/* initialise all the elements in the given arr as zero
*/
void initialise_zero(int* arr, int len){
    int i;
    for(i = 0;i<len;i++){
        arr[i] = 0;
    }
}



