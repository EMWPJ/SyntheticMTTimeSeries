Synthetic MT TimeSeries
=
This is the implementation of a synthesizing Magnetotelluric (MT) time series method based on forward modeling.

Program language
-
Delphi 10.3

Usage
-

Build the project to get the executable

Run **Synthesize_MT_Timeseries.exe**

Right-click under Sites to import the forward response file(obtained by forward acting, see https://github.com/EMWPJ/Modified-ModEM-with-output-electromagnetic-field-values for more). Select the measuring points to be synthesized.

Set synthetic time series parameters:

    Average length of each segment

    Intensity of the source (used for overall scaling time series intensity)

    Function of synthetic time series (default is random segment length and window at concatenation)

    The rotation Angle of the observation direction

Set Observation system Parameters.

Select the output in Phoenix format or text format.

Click **Generate Sites** to begin the synthesis. For each site, the synthesis process takes about a few minutes.


Executable program and example
-

See **Win64\Release**