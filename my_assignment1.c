/*
** Tessa(Hyeri) Song
** Student number : 597952
** Email address : songt@student.unimelb.edu.au
** Last edited : Thursday 25 April 2015
**
** Written for Assignment 1 of Engineering Computation(COMP20005)
** This program is basically analysing information about polygons
**      and calculating additional information 
**		such as area, perimeter and so on.
**
** Algorithms are fun
*/

#include<stdio.h>
#include<assert.h>
#include<math.h>

#define MAX_PNT 100
#define MAX_POLY 100
#define PIE 3.14159265
#define CONTINUE_INPUT 1
#define END_INPUT 0


/* Struct for storing information on polygon */
typedef struct {
	int npoints;
	int id;
	double x_arr[MAX_PNT];
	double y_arr[MAX_PNT];
	double perim;
	double area;
	double eccen;
} Poly;


/* function prototypes */
int put_info_poly(Poly * poly); 
void cal_perim(Poly * poly); 
void cal_area(Poly * poly); 
void cal_eccen(Poly * poly); 
double get_length(double x1, double x2, double y1, double y2); 
int get_largest(Poly * p_arr, int len); 



int main(){
	
	int i, len = 0;
	Poly p_arr[MAX_POLY]; 
	int lgst_idx;
	
	/* put information in the Poly array and calculate*/
	while(put_info_poly(&p_arr[len])){
		cal_perim(&p_arr[len]);
		cal_area(&p_arr[len]);
		cal_eccen(&p_arr[len]);
		len++;
	}

	
	/* Stage 1 */
	printf("\n");
	printf("Stage 1\n");
	printf("=======\n");
	printf("First polygon is %d\n", p_arr[0].id);
	printf("%8s%8s\n", "x_val", "y_val");
	
	for(i = 0;i<p_arr[0].npoints;i++){
		printf("%7.1f %7.1f\n", p_arr[0].x_arr[i], p_arr[0].y_arr[i]);
	}
	
	printf("perimeter    =%6.2f m\n", p_arr[0].perim);
	printf("area         =%6.2f m^2\n", p_arr[0].area);
	printf("eccentricity =%6.2f\n", p_arr[0].eccen);
	printf("\n");
	
	
	
	/* Stage 2 */
	printf("Stage 2\n");
	printf("=======\n");
	printf("+-------+-------+-------+-------+-------+\n");
	printf("|    id |  nval | perim |  area | eccen |\n");
	printf("+-------+-------+-------+-------+-------+\n");
	
	/*iterate the poly array*/
	for(i = 0;i<len;i++){
		printf("| %5d ", p_arr[i].id);
		printf("| %5d ", p_arr[i].npoints);
		printf("|%6.2f ", p_arr[i].perim);
		printf("|%6.2f ", p_arr[i].area);
		printf("|%6.2f |\n",p_arr[i].eccen);
	}
	
	printf("+-------+-------+-------+-------+-------+\n");
	printf("\n");
	
	
	
	/* Stage 3 */
	printf("Stage 3\n");
	printf("=======\n");
	
	/* get the largest index */
	lgst_idx = get_largest(p_arr, len); 
	printf("Largest polygon is %d\n", p_arr[lgst_idx].id);
	printf("%8s%8s\n", "x_val", "y_val");
	
	/* print all the points */
	for(i = 0;i<p_arr[lgst_idx].npoints;i++){
		printf("%7.1f %7.1f\n",p_arr[lgst_idx].x_arr[i], 
			p_arr[lgst_idx].y_arr[i]);
	}
	
	return 0;
	
}



/* put information into polygon struct */
int put_info_poly(Poly * poly){
	assert(poly);
	
	int i;
	
	/* scan the information and put it in the poly struct */
	if(scanf("%d %d", &(poly->npoints), &(poly->id)) == 2){
		
		/* scan all the points */
		for(i = 0; i<poly->npoints; i++){
			scanf("%lf", &(poly->x_arr[i]));
			scanf("%lf", &(poly->y_arr[i]));
		}
		
		return CONTINUE_INPUT;
	}
	
	return END_INPUT;
}



/* calculate parameter */
void cal_perim(Poly * poly){
	assert(poly);
	
	int npoints = poly->npoints;
	int i, next_idx;
	double perim = 0.0;

	for(i = 0;i<npoints;i++){
		
		/* set the next_idx up to i */
		next_idx = i+1;
		if(i == npoints-1)
			next_idx = 0;
		
		/* keep adding each length */
		perim += get_length(poly->x_arr[i], poly->x_arr[next_idx],
			poly->y_arr[i], poly->y_arr[next_idx]);
		
	}
	
	poly->perim = perim;
	
}



/* calculate area */
void cal_area(Poly * poly){
	assert(poly);
	
	int npoints = poly->npoints;
	int i, next_idx;
	double area = 0.0, partial;
	
	for(i = 0;i<npoints; i++){
		
		/* set the next_idx up to i */
		next_idx = i+1;
		if(i == npoints-1)
			next_idx = 0;
		
		/* calculate the partial area */
		partial = (poly->y_arr[i] + poly->y_arr[next_idx]) *
			fabs(poly->x_arr[i] - poly->x_arr[next_idx]) * 0.5;
		
		/* set the sign of the partial area and add it */		
		if(poly->x_arr[i] < poly->x_arr[next_idx]){
			area+=partial;
		}else{
			area-=partial;
		}	
		
	}
	
	poly->area = area;
	
}



/* calculate eccentricity */
void cal_eccen(Poly * poly){
	assert(poly);
	
	double perim = poly->perim;
	double area = poly->area;
	
	/* calculate using two variables above */
	double eccen = perim*perim/area/(4*PIE);
	
	poly->eccen = eccen;
	
}



/* calculate the length between two points */
double get_length(double x1, double x2, double y1, double y2){
	
	double len = sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2));
	return len;
	
}



/* return index of the largest polygon */
int get_largest(Poly * p_arr, int len){
	assert(p_arr);
	assert(len>0);
	
	int i;
	int lgst_idx = 0;
	
	for(i = 1;i<len;i++){
		
		/* compare areas */
		if (p_arr[i].area > p_arr[lgst_idx].area){
			lgst_idx = i;
			
		/* in case that two areas are the same, compare ids */	
		}else if(p_arr[i].area == p_arr[lgst_idx].area){
			if(p_arr[i].id < p_arr[lgst_idx].id){
				lgst_idx = i;
			}
		}
	}
	
	return lgst_idx;

}


