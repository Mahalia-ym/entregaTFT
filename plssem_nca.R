library(readr)
library(cSEM)
library(xlsx)
library(NCA)

datos <- "D:\\codeTFT\\BD inicial cleaned MAhalia.csv"
contentData <- read.csv2(datos)

modeloDef <- "
#Measurement models
Satisf =~ jobSatisfaction_1 + jobSatisfaction_2 + jobSatisfaction_3 + jobSatisfaction_4 + 
          jobSatisfaction_5 + jobSatisfaction_6
Mindfulness =~ mindFullness_1 + mindFullness_2 + mindFullness_3 + mindFullness_4 + mindFullness_5
Performance =~ performance_1 + performance_2 + performance_3 + performance_4
Abandon =~ abandon_1 + abandon_2 + abandon_3 + abandon_4

#Structural model
Satisf ~ Mindfulness
Performance ~ Satisf + Mindfulness
Abandon ~ Satisf + Performance
"

valorSalida <- csem(.data = contentData, .model = modeloDef, 
                    .disattenuate = FALSE, .resample_method = "bootstrap", .R=5000)

verify(valorSalida)

assess(valorSalida, "rmsea")
calculateRMSEA(valorSalida)
calculatef2(valorSalida)
predict(valorSalida)
infer(valorSalida)
summarize(valorSalida)


salidasTodas <- summarize(valorSalida)
salidasTodas

salidasTodas$Estimates$Path_estimates
write.csv(x = salidasTodas$Estimates$Path_estimates, file ="tablas/path.csv")
#Las cargas de los indicadores
salidasTodas$Estimates$Loading_estimates
write.csv(x = salidasTodas$Estimates$Loading_estimates, file = "tablas/loading.csv")

#Casi toda la información
assess(valorSalida)
exportToExcel(.postestimation_object = assess(valorSalida),
              .filename = "muchosValores.xlsx", .path = "tablas/")

exportToExcel(.postestimation_object = predict(valorSalida), .filename = "predict.xlsx", .path = "tablas/")

#modelo lineal
predict(valorSalida, .benchmark = c("lm"))
#Calcular el HTMT
valoresHTMT <- calculateHTMT(valorSalida)
valoresHTMT
write.csv(x = valoresHTMT, file = "tablas/valoresHTMT.csv")

#calcular fornell
valorFornell <- calculateFLCriterion(valorSalida)
valorFornell


#prueba <- resampleData(valorSalida)
#testOMF(valorSalida)

###################### NCA ########################
scores <- getConstructScores(.object = valorSalida,
                             .standardized = TRUE)
write.csv(x=scores$Construct_scores, file = "tablas/constructScores.csv")
puntuaciones <- scores$Construct_scores
puntuaciones

m1 <- nca_analysis(puntuaciones, "Satisf", "Performance", ceilings = 'ce_fdh', corner = 1, test.rep = 10000)
m1
nca_output(m1, plots = TRUE)
nca_output(m1, bottlenecks = TRUE)
m1_1 <- nca_analysis(puntuaciones, "Satisf", "Performance", ceilings = 'ce_fdh', corner = 4, test.rep = 10000)
m1_1
nca_output(m1_1, plots = TRUE)
nca_output(m1, bottlenecks = TRUE)

m2 <- nca_analysis(puntuaciones, "Satisf", "Abandon", ceilings = 'ce_fdh', corner = 2, test.rep = 10000)
m2
nca_output(m2, plots = TRUE)
nca_output(m2, bottlenecks = TRUE)
m2_2 <- nca_analysis(puntuaciones, "Satisf", "Abandon", ceilings = 'ce_fdh', corner = 3, test.rep = 10000)
m2_2
nca_output(m2_2, plots = TRUE)
nca_output(m2_2, bottlenecks = TRUE)

m3 <- nca_analysis(puntuaciones, "Mindfulness", "Satisf", ceilings = 'ce_fdh', corner = 1, test.rep = 10000)
m3
nca_output(m3, plots = TRUE)
nca_output(m3, bottlenecks = TRUE)
m3_3 <- nca_analysis(puntuaciones, "Mindfulness", "Satisf", ceilings = 'ce_fdh', corner = 4, test.rep = 10000)
m3_3
nca_output(m3_3, plots = TRUE)
nca_output(m3_3, bottlenecks = TRUE)

m4 <- nca_analysis(puntuaciones, "Mindfulness", "Performance", ceilings = 'ce_fdh', corner = 1, test.rep = 10000)
m4
nca_output(m4, plots = TRUE)
nca_output(m4, bottlenecks = TRUE)
m4_4 <- nca_analysis(puntuaciones, "Mindfulness", "Performance", ceilings = 'ce_fdh', corner = 4, test.rep = 10000)
m4_4
nca_output(m4_4, plots = TRUE)
nca_output(m4_4, bottlenecks = TRUE)

m5 <- nca_analysis(puntuaciones, "Performance", "Abandon", ceilings = 'ce_fdh', corner = 2, test.rep = 10000)
m5
nca_output(m5, plots = TRUE)
nca_output(m5, bottlenecks = TRUE)
m5_5 <- nca_analysis(puntuaciones, "Performance", "Abandon", ceilings = 'ce_fdh', corner = 3, test.rep = 10000)
m5_5
nca_output(m5_5, plots = TRUE)
nca_output(m5_5, bottlenecks = TRUE)
