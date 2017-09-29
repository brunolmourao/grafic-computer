#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <iostream>

using namespace std;

// Método para calcular a Matriz Inversa
double** calcularInversa(double **matriz, int n){
    int i, j, w;
    double pivo, op, **matriz2, **inversa;

    inversa = (double**)malloc(n*sizeof(double*));

    for(i=0;i<n;i++)
        inversa[i] = (double*)calloc(n,sizeof(double));

    matriz2 = (double**)malloc(n*sizeof(double*));

    for(i=0;i<n;i++)
        matriz2[i] = (double*)calloc(n,sizeof(double));

    for(i=0;i<n;i++)
        inversa[i][i] = 1;

    for(i=0;i<n;i++){
        for(j=0;j<n;j++)
            matriz2[i][j] = matriz[i][j];
    }

    for (i=0;i<n;i++){//i linha atual, j coluna
        pivo = matriz2[i][i];
        if(pivo != 1){
            for(j=0;j<n;j++){
                matriz2[i][j] = matriz2[i][j]/pivo;
                inversa[i][j] = inversa[i][j]/pivo;
            }
        }

        //w irá percorrer as linhas
        for(w=0;w<n;w++){
            if(w != i){
                op = matriz2[w][i]; //i como linha
                for(j=0;j<n;j++){

                    matriz2[w][j] = matriz2[w][j]-(op*(matriz2[i][j]));
                    inversa[w][j] = inversa[w][j]-(op*(inversa[i][j]));

                }
            }
        }
    }
	return inversa;
}
// Produto escalar de dois vetores
double produtoEscalar(double *v1, double *v2, int n){
    double v=0;

    for(int i=0;i<n;i++)
        v += v1[i]*v2[i];

    return v;
}
// Normalizar um vetor
double* normalizarVetor(double *v, int n){

    double modulo=0, *v1;

    v1 = (double*)calloc((n),sizeof(double));

    for(int i=0;i<n;i++){
        modulo += pow(v[i],2);
    }
    modulo = sqrt(modulo);

    for(int i=0;i<n;i++){
        v1[i] = v[i]/modulo;
    }

    return v1;
}
// Norma de um vetor
double norma(double *v, int n){

    double modulo=0;

    for(int i=0;i<n;i++){
        modulo += pow(v[i],2);
    }
    modulo = sqrt(modulo);

    return modulo;
}
// Multiplicação Matriz por Vetor
double* multMatrizVetor(double **m1, double *v, int tam1i, int tam1j, int tam2i){

    int i, w;
    double *v2;

    v2 = (double*)calloc((tam1i),sizeof(double*));

    if(tam1j==tam2i){
        for(w=0;w<tam1i;w++){
            for(i=0;i<tam2i;i++) {
                v2[w] += (m1[w][i])*(v[i]);
            }
        }
    }

    return v2;
}
// Subtrair matriz m2 da matriz m1
double** subMatriz(double **m1, double **m2, int tam){

    double **m3;

    m3 = (double**)malloc((tam)*sizeof(double*));

    for(int i=0;i<tam;i++)
        m3[i] = (double*)calloc((tam),sizeof(double));

    for(int w=0;w<tam;w++){
        for(int i=0;i<tam;i++) {
            m3[w][i] = (m1[w][i])-(m2[w][i]);
        }
    }
    return m3;
}

// Multiplica matriz m1 por escalar
double** multMatrizEscalar(double **m1, double esc, int tam1i, int tam1j){

    double **m2;

    m2 = (double**)calloc((tam1j),sizeof(double));

    for(int i=0;i<tam1j;i++)
        m2[i] = (double*)calloc((tam1j),sizeof(double));

    for(int w=0;w<tam1i;w++){
        for(int i=0;i<tam1j;i++){
                m2[w][i] = m1[w][i]*esc;
        }
    }
    

    return m2;
}

double** multMatriz(double **m1, double **m2, int tam1i, int tam1j, int tam2i, int tam2j){

    int i, j, w;
    double **m3, e;

    e = pow(10,-12);
    m3 = (double**)malloc((tam1i)*sizeof(double*));

    for(i=0;i<tam1i;i++)
        m3[i] = (double*)calloc((tam2j),sizeof(double));

    if(tam1j==tam2i){

        for(w=0;w<tam1i;w++){
            for(i=0;i<tam1j;i++) {
                for(j=0;j<tam2j;j++){
                    m3[w][j] += (m1[w][i])*(m2[i][j]);
                    if(fabs(m3[w][j])<e)
                        m3[w][j] = 0;
                }
            }
        }
    }
    return m3;
}
double** matrizTransposta(double **A, int n){
    double **A2, aux;

    A2 = (double**)malloc((n)*sizeof(double*));

    for(int i=0;i<n;i++)
        A2[i] = (double*)calloc((n),sizeof(double));

    /*for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
            A2[i][j] = A[i][j];
        }
    }*/

    for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
            /*aux = A[i][j];
            A2[i][j] = A[j][i];
            A2[j][i] = aux;*/
            A2[i][j] = A[j][i];
        }
    }

    return A2;
}
double** getIdentidade(int n){
    double **identidade;
    identidade = (double**)calloc((n),sizeof(double));
    for(int i=0;i<n;i++){
        identidade[i] = (double*)calloc((n),sizeof(double));
    }
    
    for(int i=0;i<n;i++){
        for(int j=0;j<n;j++){
            if(i==j)
                identidade[i][j] = 1.0;
            else
                identidade[i][j] = 0.0;
        }
    }
    return identidade;
}
double** matrizEscala(int n,double *v){
    double **matrizEscala;
    matrizEscala = getIdentidade(n);
    for(int i=0;i<n;i++){
        matrizEscala[i][i] = v[i]; 
    }
    return matrizEscala;
}
double** matrizRotacaoEixoX(int n,double angle){
    double **matrizRotacao;
    double angleRad = (angle * (M_PI) / 180.0);
    matrizRotacao = getIdentidade(n);
    matrizRotacao[1][1] = cos(angleRad);
    matrizRotacao[2][1] = sin(angleRad);
    matrizRotacao[1][2] = -sin(angleRad);
    matrizRotacao[2][2] = cos(angleRad);
    return matrizRotacao;

}
double** matrizRotacaoEixoY(int n,double angle){
    double **matrizRotacao;
    double angleRad = (angle * (M_PI) / 180.0);
    matrizRotacao = getIdentidade(n);
    matrizRotacao[0][0] = cos(angleRad);
    matrizRotacao[0][2] = sin(angleRad);
    matrizRotacao[2][2] = -sin(angleRad);
    matrizRotacao[2][0] = cos(angleRad);
    return matrizRotacao;

}
double** matrizRotacaoEixoZ(int n,double angle){
    double **matrizRotacao;
    double angleRad = (angle * (M_PI) / 180.0);
    matrizRotacao = getIdentidade(n);
    matrizRotacao[0][0] = cos(angleRad);
    matrizRotacao[1][0] = sin(angleRad);
    matrizRotacao[0][1] = -sin(angleRad);
    matrizRotacao[1][1] = cos(angleRad);
    return matrizRotacao;

}
double** matrizTranslacao(int n,double *v){
    double **matrizTranslacao;
    matrizTranslacao = getIdentidade(n);
    for(int i=0;i<n-1;i++){
        matrizTranslacao[i][3] = v[i];
    }
    return matrizTranslacao;
}
double** matrizEspelho(int n,int eixo){ //xy =0 , xz = 1 , yz = 2
    double **matrizEspelho;
    matrizEspelho = getIdentidade(n);
    if(eixo == 0){
        matrizEspelho[2][2] = -1;
    }else{
        if(eixo == 1){
            matrizEspelho[1][1] = -1;
        }
        else{
            matrizEspelho[0][0] = -1;
        }
    }
    return matrizEspelho;
}
double** matrizCisalhamento(int n,double angle,int eixo){ //xy = 0,xz = 1, zx = 2, zy = 3
    double **matrizCisalhamento;
    matrizCisalhamento = getIdentidade(n);
    double angleRad = (angle * (M_PI) / 180.0);
    if(eixo == 0){
        matrizCisalhamento[1][0] = tan(angleRad);
    }
    if(eixo == 1){
        matrizCisalhamento[2][0] = tan(angleRad);
    }
    if(eixo == 2){
        matrizCisalhamento[0][2] = tan(angleRad);
    }
    if(eixo == 3){
        matrizCisalhamento[1][2] = tan(angleRad);
    }
    return matrizCisalhamento;
    
}
int main(int argc, char *argv[]){
    double **T,**E;
    double *v;
    int n = 4;
    v = (double*)calloc((n),sizeof(double*));
    v[0] = 3;
    v[1] = 4;
    v[2] = 5;
        T = matrizTranslacao(n,v);
        E = matrizCisalhamento(n,60,2);
    for(int i=0;i<n;i++){
        cout<<"|";
        for(int j=0;j<n;j++){
            cout<<E[i][j]<<" ";
        }
        cout<<"|"<<endl;
    }
    return 0;
}
