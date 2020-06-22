# Monetary Policy Evaluation - Bernanke et al. (2005) Revisited

Here you can find supporting documentation for the Paper *"Monetary Policy Evaluation with Data Irregularities: A Factor Augmented Vector Autoregression Analysis"*. This Paper started as my bachelor thesis at the University of Mannheim. I currently see it as an open project/working paper with no intentions for publication in this form.

The files are divided into three classes:
<ul style="list-style-type:none;">
  <li>Files beginning with "A" are .Rdata files that contain the raw data or data for the graphs</li>
  <li>Files beginning with "CODE" are .R files that contain replicable R-code</li>
  <li>Files beginning with "GRAPHS" are .Rmd files that can be executed to get the graphs from the thesis</li>
 </ul>

To replicate the graphs from my paper, you have two options:
<ol>
  <li>Directly run the "GRAPHS" files to get the graphs based on the data stored in the "A" files</li>
  <li>Run (and feel free to inspect/change) the "CODE"-files to see how the graphs are constructed and subsequently run the "GRAPHS" files if you wish so</li>
</ol>

Feel free to open an issue or send me an [email](mailto:truesch@mail.uni-mannheim.de) when you have questions or find errors in my analysis.<br/>
You can find the submitted version of the paper [here](https://drive.google.com/open?id=1icB47gSR78lk2RCmMOmKI3nXT2zK_EMR).
A current working paper version is available [here](https://drive.google.com/file/d/1Xy2NUWD3_4afH7hcLdh-iq7G5rVSayEO/view?usp=sharing).

### Abstract

I examine the methodology and economic theory behind the FAVAR approach from Bernanke, Boivin, and Eliasz (2005) and study its application in an empirical analysis on the effects of monetary policy for Germany. While the basic tools used are not new, this paper focuses on the empirical implementation and extends the analysis of Bernanke, Boivin, and Eliasz (2005) to unbalanced panels. I use an EM-algorithm in combination with principal component estimation for the economic factors to handle mixed frequencies and missing observations, which potentially improves the accuracy of the dynamic responses as it allows the econometrician to use a broader data set and produces monthly impulse responses. The responses are robust to the number of factors and the lag-length and to the choice of the monetary policy tool.
