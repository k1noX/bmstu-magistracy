#include<stdio.h>
#include<sys/time.h>

#define M 100

double MA[M][M+1], MAD;

int main() {
    int i, j, v, k;

    struct timeval tv1, tv2; 
    long int dt1; 

    for(i = 0; i < M; i++) {
        for(j = 0; j < M; j++) { 
            if(i == j) MA[i][j] = 2.0;
            else MA[i][j] = 1.0;
        }
        MA[i][M] = 1.0*(M)+1.0;
    }

    gettimeofday(&tv1,NULL);

    for(k = 0; k < M; k++) {
        MAD = 1.0/MA[k][k];
        for(j = M; j >= k; j--) MA[k][j] *= MAD;
        for(i = k+1; i < M; i++)
            for(j = M; j >= k; j--)
                MA[i][j] -= MA[i][k]*MA[k][j];
                
    }  

    for(k = M-1; k >= 0; k--)
        for(i = k-1; i >= 0; i--)
            MA[i][M] -= MA[k][M]*MA[i][k];  

    gettimeofday(&tv2,NULL);
    dt1 = (tv2.tv_sec - tv1.tv_sec)*1000000 + tv2.tv_usec - tv1.tv_usec;
    printf("Time = %ld\n",dt1);

    printf(" %f %f %f %f\n",MA[0][M],MA[1][M],MA[2][M],MA[3][M]);
    printf(" %f %f %f %f\n",MA[4][M],MA[5][M],MA[6][M],MA[7][M]);

    return(0);
}
