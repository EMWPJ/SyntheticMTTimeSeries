Synthetic MT TimeSeries
=
This is the implementation of a synthesizing Magnetotelluric (MT) time series method based on forward modeling.

Program language
-
Delphi 10.3

Usage
-

1. Build the project to get the executable
2. Run **Synthesize_MT_Timeseries.exe**
3. Right-click under Sites to import the forward response (obtained by forward acting, see https://github.com/EMWPJ/Modified-ModEM-with-output-electromagnetic-field-values for more).
4. Set synthetic time series parameters:
   3.1 Average length of each segment
   3.2 Intensity of the source (used for overall scaling time series intensity)
   3.3 Function of synthetic time series (default is random segment length and window at concatenation)
   3.4 The rotation Angle of the observation direction
5. Observation system Parameters
6. Click **Generate Sites** to begin the synthesis. For each site, the synthesis process takes about a few minutes.


Executable program and example
-

See **Win64\Release**