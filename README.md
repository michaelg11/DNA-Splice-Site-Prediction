# DNA-Splice-Site-Prediction

One of the important steps in transfer of biological sequential information is gene splicing. Several challenges
hinder study of gene splicing, such as lack of labeled data which can be used to identify splice sites. Like many other applications in biological problems, machine learning has been used to identify these splice sites. The lack of labeled data, however, makes it a difficult task to build supervised classifiers which can accurately predict these splice sites. We have aimed to address this problem by integrating gene splice site domain knowledge with machine learning techniques.

1. Data availability: Data set used is available at http://cbio.mskcc.org/public/raetschlab/user/cwidmer/
   Download the apppropriate dataset and run the codes.
2. The feature generation file is used to generate DNA-related features which can help in splice site detection. The details can be found in the following paper-
   Islamaj, R., Getoor, L., & Wilbur, W. J. (2006, September). A feature generation algorithm for sequences with application to splice-site prediction. In European Conference on Principles of Data Mining and Knowledge Discovery (pp. 553-560). Springer Berlin Heidelberg.
3. The feature selection methods used are dependent on the type of Machine Learning technique used, and are different for different algorithms. Before building the models, feature selection is used in each of the codes.
4. Since this is a rare event problem, we have used the Precision-Recall Curve as the validation criteria, and have compared models based on that.

The work was inspired from the following paper-
Herndon, N., & Caragea, D. (2016). A Study of Domain Adaptation Classifiers Derived From Logistic Regression for the Task of Splice Site Prediction. IEEE transactions on nanobioscience, 15(2), 75-83.

Our work is in the process of being published, as we are still working on compiling the reports and the resutls, and improving on some of them. 
