#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <stdlib.h>

using namespace std;

#define Matrix vector<vector <double> >
#define Vector vector <double>

Matrix zeroMatrix (int n)
{
    Matrix m;
    for (int i = 0; i < n; i++) {
        Vector zeroV;
        for (int j = 0; j < n; j++) {
            zeroV.push_back(0);
        }
        m.push_back(zeroV);
    }
    return m;
}

void showMatrix (Matrix m)
{
    for (int i = 0; i < m.size(); i++) {
        for (int j = 0; j < m[0].size(); j++) {
            cout << m[i][j] << ' ';
        }
        cout << endl;
    }
}

int main ()
{
  string line;
  string fileName = "file.txt";
  ifstream f(fileName);

  // Infer the dimensionality of the resulting matrix (n x n)
  int n;
  if (f.is_open())
  {
    getline(f,line);
    Vector v;

    string buf;
    stringstream ss(line);

    while (ss >> buf)
    {
        v.push_back(atof(buf.c_str()));
    }
    n = v.size();
    f.close();
  }
  Matrix matrix = zeroMatrix(n);

  f.clear();
  f.open(fileName);
  if (f.is_open())
  {
    while (getline(f,line))
    {
      Vector v;

      string buf;
      stringstream ss(line);

      while (ss >> buf)
      {
        v.push_back(atof(buf.c_str()));
      }

      Matrix matrixTmp;
      for (int i = 0; i < n; i++) {
          double el = v[i];
          Vector tmp;
          for (int j = 0; j < n; j++) {
              // Calculate partial product
              // and add accumulated result.
              tmp.push_back(el * v[j] + matrix[i][j]);
          }
          matrixTmp.push_back(tmp);
      }
      matrix = matrixTmp;
    }
    f.close();

    showMatrix(matrix);
  }
  else cout << "Unable to open file";

  return 0;
}
