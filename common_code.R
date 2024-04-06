#código comum

media_5_unidas <- cbind(media_5, media_5_P)

# Instalando a biblioteca 'writexl'
install.packages("writexl")

# Carreguando a biblioteca 'writexl'
library(writexl)

# Escrevendo a tabela para um arquivo Excel
write_xlsx(media_5_unidas, "media_5_unidas.xlsx")

