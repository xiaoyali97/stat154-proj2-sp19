# stat154-proj2-sp19

### Step to reproduce this project
1. Clone this repository
2. Create a `/data` folder and put all image.txt files in the folder
3. The `utils.R` in the `/code` folder contains all functions we used to perform model fitting, including the `CVgeneric` function that is required in 2d).
3. In the `/analysis` folder:
  * `data_preparation.Rmd` and `data_preparation_v2.Rmd` contains code that preprocesses the image data
  * `models_analysis.Rmd` contains code that fits models(logistic, LDA, QDA) and further model diagnostic
  * `KNN_analysis.Rmd` contains code that fits KNN models
  * `QDA_diagonstic.Rmd`contains code that analysis the QDA model and adding new features by Using EM algorithm
