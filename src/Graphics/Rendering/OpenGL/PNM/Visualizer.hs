module Graphics.Rendering.OpenGL.PNM.Visualizer
where

import Data.Bits
import System.IO
import Graphics.UI.GLUT

{-
#include <complex>
#include <fstream>

using namespace std;

const double PI = 3.1415926535897932384626433832795;
const double E  = 2.7182818284590452353602874713527;

void SetHSV(double h, double s, double v, unsigned char color[3]) {
    double r, g, b;
    if(s==0)
        r = g = b = v;

    else {
        if(h==1) h = 0;
        double z = floor(h*6); int i = int(z);
        double f = double(h*6 - z);
        double p = v*(1-s);
        double q = v*(1-s*f);
        double t = v*(1-s*(1-f));

        switch(i){
        case 0: r=v; g=t; b=p; break;
        case 1: r=q; g=v; b=p; break;
        case 2: r=p; g=v; b=t; break;
        case 3: r=p; g=q; b=v; break;
        case 4: r=t; g=p; b=v; break;
        case 5: r=v; g=p; b=q; break;
        }
    }
    int c;
    c = int(256*r); if(c>255) c = 255; color[0] = c;
    c = int(256*g); if(c>255) c = 255; color[1] = c;
    c = int(256*b); if(c>255) c = 255; color[2] = c;
}

complex<double> fun(complex<double>& c ){
    const complex<double> i(0., 1.);
    return (pow(c,2) -1.) *pow(c -2. -i, 2) /(pow(c,2) +2. +2. *i);
}

int main(){
    const int dimx = 800; const int dimy = 800;
    const double rmi = -3; const double rma =  3;
    const double imi = -3; const double ima =  3;

    ofstream f("complex.ppm", ios::binary);
    f << "P6" << endl
      << dimx << " " << dimy << endl
      << "255" << endl;

    for(int j=0; j < dimy; ++j){
        double im = ima - (ima -imi) *j /(dimy -1);
        for(int i=0; i < dimx; ++i){
            double re = rma -(rma -rmi) *i /(dimx -1);
            complex<double> c(re, im);
            complex<double> v = fun(c);
            double a = arg(v);

            while(a<0) a += 2*PI; a /= 2*PI;
            double m = abs(v);
            double ranges = 0;
            double rangee = 1;

            while(m>rangee){
                ranges = rangee;
                rangee *= E;
            }

            double k   = (m-ranges)/(rangee-ranges);
            double sat = k < 0.5 ? k *2: 1 -(k -0.5) *2;
            sat = 1 - pow(1-sat, 3); sat = 0.4 + sat*0.6;

            double val = k < 0.5 ? k *2: 1 -(k -0.5) *2; val = 1 - val;
            val = 1 - pow(1-val, 3); val = 0.6 + val*0.4;

            unsigned char color[3];
            SetHSV(a,sat,val,color);
            f.write((char const *)color,3);
        }
    }
    return 0;
}
-}
