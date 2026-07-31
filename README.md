# ISPaL Shiny

Interface Shiny para executar e explorar previsões periódicas com os modelos
PAR, PARX e RIDGE associados ao pacote ISPaL.

A aplicação é autocontida dentro desta pasta. Ela não depende de arquivos em
diretórios externos para executar, testar ou carregar os dados de exemplo.

## Funcionalidades

- Importação de CSV, TXT, TSV, XLS e XLSX.
- Uso de uma tabela conjunta ou de tabelas Y e X separadas.
- Seleção de uma ou várias séries dependentes Y.
- Seleção de até 10 covariáveis X.
- Identificação automática das colunas de data, Y e X, com alteração opcional
  pelo Diagnóstico.
- Conversão e validação de datas mensais.
- Escolha de modelos, leads, meses e períodos.
- Execução com barra de progresso e captura de erros.
- Visualização interativa das quatro tabelas:
  - `Forecast.table`;
  - `Error.table`;
  - `All.error.table`;
  - `Lambda.table`.
- Exportação das quatro tabelas em um único arquivo Excel.
- Gráficos de distribuição/ridgelines e boxplots.
- Gráficos de modelo vencedor por mês ou lead.
- Matriz de correlação das covariáveis.
- Correlação cruzada (CCF) entre uma ou várias variáveis Y e as covariáveis X.
- Geração dos gráficos a partir de uma tabela de resultados já existente.

## Instalação

No R:

```r
setwd("/caminho/para/ISPaL_Shiny")
source("install_dependencies.R")
```

Dependências principais:

`shiny`, `bslib`, `shinyWidgets`, `DT`, `readxl`, `readr`, `openxlsx`,
`ggplot2`, `ggridges`, `patchwork`, `shinycssloaders`, `dplyr`,
`lubridate`, `MASS`, `scales` e `testthat`.

## Como publicar no GitHub

Use o conteúdo da pasta `ISPaL_Shiny` como a raiz do repositório. Ou seja,
suba estes arquivos e pastas diretamente:

```text
app.R
R/
www/
inst/
tests/
README.md
run_app.R
install_dependencies.R
LICENSE
.gitignore
```

Não é necessário subir a pasta externa `ISPaL (fonte)`. Os dados de exemplo
usados pelos testes e pela documentação já estão em `inst/extdata`.

## Como executar

Dentro da pasta `ISPaL_Shiny`:

```r
shiny::runApp(".")
```

ou:

```r
source("run_app.R")
```

A partir da pasta que contém `ISPaL_Shiny`:

```r
shiny::runApp("ISPaL_Shiny")
```

Após clonar ou baixar o repositório do GitHub, entre na pasta baixada, instale
as dependências uma vez e execute:

```r
source("install_dependencies.R")
source("run_app.R")
```

## Formato dos dados

### Uma tabela

```text
Date        Y_1     Y_2     X_1     X_2
1949-01-01  ...     ...     ...     ...
1949-02-01  ...     ...     ...     ...
```

O usuário indica:

- a coluna de data;
- uma ou mais colunas Y;
- zero a dez colunas X.

Uma coluna não pode ser simultaneamente Y e X.

### Duas tabelas

Tabela Y:

```text
Date        Y_1     Y_2
1949-01-01  ...     ...
```

Tabela X:

```text
Date        X_1     X_2     X_3
1949-01-01  ...     ...     ...
```

As tabelas podem começar em anos diferentes, mas precisam cobrir os períodos
necessários à execução selecionada.

### Dados de exemplo incluídos

Os arquivos de exemplo estão dentro da própria aplicação:

- `inst/extdata/data/StreamflowEnergy.rda`;
- `inst/extdata/data/ClimaticInfo.rda`;
- `inst/extdata/example_input/Streamflow Equivalent Energy.txt`;
- `inst/extdata/example_input/Climatic indicators.txt`;
- `inst/extdata/example_output/Forecast.txt`;
- `inst/extdata/example_output/Errors.txt`;
- `inst/extdata/example_output/All Errors.txt`;
- `inst/extdata/example_output/Lambda.txt`.

## Datas

Formatos aceitos:

- `YYYY-MM-DD`;
- `DD/MM/YYYY`;
- `MM/DD/YYYY`;
- `YYYY-MM`;
- `DD-MM-YYYY`;
- `YYYY/MM/DD`;
- números de data do Excel.

A aplicação verifica datas inválidas, duplicadas, meses ausentes e frequência
mensal. Problemas não são corrigidos silenciosamente.

## Parâmetros iniciais

```r
models = c("PAR", "PARX", "RIDGE")
forecast.lag = 1:6
forecast.months = 1:12
period.calibration = c("1949-01-01", "1990-12-31")
period.validation = c("1991-01-01", "2010-12-31")
period.test = c("2011-01-01", "2021-12-31")
```

O período de validação é usado para selecionar a melhor combinação de
covariáveis do PARX. O teste é a avaliação final. PAR e RIDGE são calibrados
usando a calibração mais a validação.

## Modelos

- **PAR:** regressão periódica usando o valor defasado da própria série.
- **PARX:** acrescenta covariáveis e escolhe a combinação com maior KGE no
  período de validação.
- **RIDGE:** regressão periódica com regularização L2; lambda é escolhido pelo
  menor GCV.

PAR pode ser executado sem covariáveis. PARX e RIDGE precisam de ao menos uma
variável X.

## Até 10 covariáveis

O backend da interface foi generalizado para até 10 covariáveis sem modificar
os arquivos originais do pacote. Para manter a lógica PARX original, todos os
subconjuntos possíveis são comparados:

- 3 covariáveis: 8 combinações;
- 6 covariáveis: 64 combinações;
- 10 covariáveis: 1.024 combinações.

Por isso, várias séries Y combinadas com muitos leads, meses e dez covariáveis
podem exigir tempo considerável.

`All.error.table` inclui:

- os modelos principais selecionados;
- `PARX0`, sem covariáveis;
- um PARX forçado para cada covariável, identificado por
  `PARX_<nome_da_variável>`.

As colunas `X1_Name` a `X10_Name` documentam o mapeamento dos coeficientes
`CoefBX1` a `CoefBX10`.

## Resultados

As tabelas possuem paginação e ordenação. Um único botão exporta `Forecast`,
`Errors`, `All Errors` e `Lambda` para planilhas separadas no mesmo arquivo
Excel.

## Gráficos

### Distribuições e boxplots

O gráfico principal segue a figura utilizada no README e no artigo:

- ridgelines por modelo;
- boxplots horizontais;
- cores consistentes;
- linha vermelha no valor ideal;
- painéis para as métricas selecionadas.

Valores ideais:

| Métrica | Ideal |
|---|---:|
| NSE, KGE | 1 |
| β₍NSE₎ | 0 |
| α₍KGE₎ | 1 |
| β₍KGE₎ | 1 |
| r | 1 |

### Modelo vencedor

Conta, por mês ou lead, qual modelo está mais próximo do valor ideal para cada
métrica.

### Resultados prontos

É possível carregar `Error.table` ou `All.error.table` em TXT, CSV ou XLSX.
A interface reconhece `Model`/`Modelo`, `Lag`/`Lead`, `Month`/`Mes` e permite
mapear manualmente essas colunas.

## Testes

Na pasta do aplicativo:

```r
source("tests/testthat.R")
```

Os testes cobrem leitura de arquivos, datas, validações, múltiplas Y, execução
dos modelos, dez covariáveis, quatro tabelas, exportações e gráficos.

## Citação

Lappicy, T., & Lima, C. H. (2023). Enhancing monthly streamflow forecasting
for Brazilian hydropower plants through climate index integration with
stochastic methods. *RBRH, 28*, e48.
https://doi.org/10.1590/2318-0331.282320230118

Treistman, F., Penna, D. D. J., Khenayfis, L. D. S., Cavalcante, N. B. R.,
Souza Filho, F. D. A. D., Rocha, R. V., Estácio, A. B., Rolim, L. Z. R.,
Pontes Filho, J. D. A., Porto, V. C., Guimarães, S. O., Pessanha, J. F. M.,
Almeida, V. A., Chan, P. D. S. C., Lappicy, T., Lima, C. H. R.,
Detzel, D. H. M., & Bessa, M. R. (2023). A framework to evaluate and compare
synthetic streamflow scenario generation models. *RBRH, 28*, e43.
https://doi.org/10.1590/2318-0331.282320230115

## Licença

O pacote ISPaL original usa a licença MIT. Consulte
https://github.com/Lappicy/ISPaL.
