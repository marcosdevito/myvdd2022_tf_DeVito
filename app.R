# ------------------------------------------------------------------------------
# MVDD - TRABAJO FINAL
# Autor: Marcos De Vito
# Descripción:  Aplicación Shiny para analizar las respuestas a una encuesta de los
#               estudiantes de la Escuela de Ing. Industrial (EII) de la FCEIA-UNR.
# ------------------------------------------------------------------------------



# Paquetes necesarios
library(shiny)
library(shinythemes)
library(dplyr)
library(utils)
library(tidyr)
library(readr)
library(readxl)
library(tidyr)
library(data.table)
library(haven)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)

# 1- Importación de archivos de interés
rtas <- read_delim("2022_02.txt", delim = "|",col_names = TRUE) # archivo de respuestas de encuestas
asignaturas <- read_delim("asignaturas.txt", delim = "\t", col_names = TRUE) # archivo con info de cada asignatura

# 2- Limpieza y ordenamiento de los datos
# 2.1- Cambiamos los nombres de las columnas por el código de pregunta
rtas <- dplyr::rename(rtas,
                      Act_curricular = colnames(rtas)[4],
                      A1 = colnames(rtas)[8], 
                      A2 = colnames(rtas)[9], 
                      A3 = colnames(rtas)[10],
                      B1 = colnames(rtas)[11],
                      B2 = colnames(rtas)[12],
                      B3 = colnames(rtas)[13],
                      B4 = colnames(rtas)[14],
                      C1 = colnames(rtas)[15],
                      C2 = colnames(rtas)[16],
                      C3 = colnames(rtas)[17],
                      D1 = colnames(rtas)[18],
                      D2 = colnames(rtas)[19],
                      D3 = colnames(rtas)[20],
                      D4 = colnames(rtas)[21],
                      E1 = colnames(rtas)[22],
                      E2 = colnames(rtas)[23],
                      E3 = colnames(rtas)[24],
                      E4 = colnames(rtas)[25],
                      F1 = colnames(rtas)[26],
                      F2 = colnames(rtas)[27],
                      F3 = colnames(rtas)[28],
                      F4 = colnames(rtas)[29],
                      G1 = colnames(rtas)[30],
                      G2 = colnames(rtas)[31],
                      G3 = colnames(rtas)[32],
                      G4 = colnames(rtas)[33],
                      H1 = colnames(rtas)[34],
                      H2 = colnames(rtas)[35],
                      H3 = colnames(rtas)[36]
)

# 2.2- Reemplazamos NA por "No contesta"
rtas <- replace(rtas, is.na(rtas), "No contesta")

# 2.3- Unificamos respuestas de los 2 planes de la misma asignatura PCP
rtas <- rtas%>% 
  mutate(
    Act_curricular=ifelse(Act_curricular=="Planificación y Control de la Producción (I9636)",
                          "Planificación y control de la producción (I29)",
                          Act_curricular)
  )

# 2.4- Transformamos en factor las variables categóricas de columnas A1 a H3
rtas <- rtas %>% 
  mutate(
    across(
      .cols = A1:H3, 
      .fns = factor
    )
  )

# 2.5- Agregamos variables de interés (fionamos datasets)
rtas <- dplyr::left_join(rtas, asignaturas, by="Act_curricular",keep=FALSE) # agregamos a la tabla de respuestas info de las asignaturas



Deptos <- unique(asignaturas$Departamento)
escala_acuerdo <- c("En total desacuerdo","En desacuerdo","Neutro","De acuerdo","Completamente de acuerdo", "No contesta")


# 3- Definición la interfaz de usuario (UI)
MiInterfaz <- fluidPage(
  theme = shinytheme("cosmo"),

  # Título de la aplicación
  titlePanel("Encuestas EII - 2° cuatrimestre de 2022"),

  # Personalización de la barra lateral
  sidebarLayout(

    # Panel lateral
    sidebarPanel(
      width = 3,

      # Entrada: Elegir Departamento
      selectInput(
        inputId = "depto_elegido",
        label = "Departamento",
        choices = Deptos
      ),
      br(),
    ),

    # Panel principal
    mainPanel(
      width = 9,

      # Salidas en pestañas
      tabsetPanel(
        tabPanel(
          "Toda la EII",
          br(),
          h3(htmlOutput("cant_resp_EII")),
          h3(htmlOutput("cant_resp_depto_elegido")),
          br(),
          fluidRow(
            column(5, tableOutput("tabla_n_deptos")),
            column(7, DTOutput("tabla_n_asign", width = "90%"))
          ),
          h3(htmlOutput("titulo_fechas_resp")),
          plotOutput("dias_eii", width = "95%"),
          plotOutput("dias_deptos", width = "95%")
        ),
        tabPanel(
          "A. Aprendizaje",
          br(),
          h4(htmlOutput("CategoriaA")),
          h4(htmlOutput("SubcategoriasA")),
          h4(htmlOutput("descr_subcatA")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatA")),
          br(),
          plotOutput("subcatA_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIA")),
          br(),
          fluidRow(
            column(5, plotOutput("A_depto_elegido", width = "100%")),
            column(7, plotOutput("A_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosA")),
          br(),
          fluidRow(plotOutput("A_Deptos", width = "100%"))
        ),
        tabPanel(
          "B. Entusiasmo",
          br(),
          h4(htmlOutput("CategoriaB")),
          h4(htmlOutput("SubcategoriasB")),
          h4(htmlOutput("descr_subcatB")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatB")),
          br(),
          plotOutput("subcatB_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIB")),
          br(),
          fluidRow(
            column(5, plotOutput("B_depto_elegido", width = "100%")),
            column(7, plotOutput("B_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosB")),
          br(),
          fluidRow(plotOutput("B_Deptos", width = "100%"))
        ),
        tabPanel(
          "C. Organización",
          br(),
          h4(htmlOutput("CategoriaC")),
          h4(htmlOutput("SubcategoriasC")),
          h4(htmlOutput("descr_subcatC")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatC")),
          br(),
          plotOutput("subcatC_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIC")),
          br(),
          fluidRow(
            column(5, plotOutput("C_depto_elegido", width = "100%")),
            column(7, plotOutput("C_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosC")),
          br(),
          fluidRow(plotOutput("C_Deptos", width = "100%"))
        ),
        tabPanel(
          "D. Interacción con el grupo",
          br(),
          h4(htmlOutput("CategoriaD")),
          h4(htmlOutput("SubcategoriasD")),
          h4(htmlOutput("descr_subcatD")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatD")),
          br(),
          plotOutput("subcatD_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIID")),
          br(),
          fluidRow(
            column(5, plotOutput("D_depto_elegido", width = "100%")),
            column(7, plotOutput("D_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosD")),
          br(),
          fluidRow(plotOutput("D_Deptos", width = "100%"))
        ),
        tabPanel(
          "E. Actitud Personal",
          br(),
          h4(htmlOutput("CategoriaE")),
          h4(htmlOutput("SubcategoriasE")),
          h4(htmlOutput("descr_subcatE")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatE")),
          br(),
          plotOutput("subcatE_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIE")),
          br(),
          fluidRow(
            column(5, plotOutput("E_depto_elegido", width = "100%")),
            column(7, plotOutput("E_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosE")),
          br(),
          fluidRow(plotOutput("E_Deptos", width = "100%"))
        ),
        tabPanel(
          "F. Contenido",
          br(),
          h4(htmlOutput("CategoriaF")),
          h4(htmlOutput("SubcategoriasF")),
          h4(htmlOutput("descr_subcatF")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatF")),
          br(),
          plotOutput("subcatF_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIF")),
          br(),
          fluidRow(
            column(5, plotOutput("F_depto_elegido", width = "100%")),
            column(7, plotOutput("F_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosF")),
          br(),
          fluidRow(plotOutput("F_Deptos", width = "100%"))
        ),
        tabPanel(
          "G. Evaluaciones",
          br(),
          h4(htmlOutput("CategoriaG")),
          h4(htmlOutput("SubcategoriasG")),
          h4(htmlOutput("descr_subcatG")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatG")),
          br(),
          plotOutput("subcatG_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIG")),
          br(),
          fluidRow(
            column(5, plotOutput("G_depto_elegido", width = "100%")),
            column(7, plotOutput("G_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosG")),
          br(),
          fluidRow(plotOutput("G_Deptos", width = "100%"))
        ),
        tabPanel(
          "H. Trabajos y Materiales",
          br(),
          h4(htmlOutput("CategoriaH")),
          h4(htmlOutput("SubcategoriasH")),
          h4(htmlOutput("descr_subcatH")),
          br(),
          h4(htmlOutput("titulo_seccion_subcatH")),
          br(),
          plotOutput("subcatH_depto_elegido"),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsEIIH")),
          br(),
          fluidRow(
            column(5, plotOutput("H_depto_elegido", width = "100%")),
            column(7, plotOutput("H_EII", width = "100%"))
          ),
          br(),
          h4(htmlOutput("titulo_seccion_DeptovsDeptosH")),
          br(),
          fluidRow(plotOutput("H_Deptos", width = "100%"))
        ),
        tabPanel(
          "Información General",
          br(),
          h4(htmlOutput("info_gral"))
        )
      )
    )
  )
)

# 4- Definición del Servidor 
MiServidor <- function(input, output) {
  depto_react <- reactive({
    input$depto_elegido
  })

  # Pestaña General
  output$cant_resp_EII <- renderText({
    paste("Cantidad total de respuestas para toda la EII: <b>", count(rtas), "</b>")
  })
  output$cant_resp_depto_elegido <- renderText({
    paste0(
      "Cantidad total de respuestas para el Departamento de <b>",
      depto_react(),
      "</b>: <b>",
      rtas %>% filter(Departamento == depto_react()) %>% count(),
      "</b>"
    )
  })
  output$tabla_n_deptos <- renderTable({
    rtas %>%
      group_by(Departamento) %>%
      dplyr::summarise(`Cantidad de respuestas` = n()) %>%
      arrange(desc(`Cantidad de respuestas`))
  })
  output$tabla_n_asign <- DT::renderDT({
    DT::datatable(
      data = count(rtas, rtas$Act_curricular, sort = TRUE, name = "Cantidad de respuestas"),
      colnames = c("Actividad curricular", "Cantidad de respuestas")
    )
  })
  output$titulo_fechas_resp <- renderText({
    "Cantidad de respuestas por día en período de clases y de mesas de examen"
  })
  output$dias_eii <- renderPlot({
    rtas %>%
      ggplot() +
      aes(x = `Fecha de inicio`) +
      annotate("rect",
        xmin = as.Date("2022-11-28"),
        xmax = as.Date("2022-12-12"),
        ymin = 0,
        ymax = Inf,
        fill = "#00b7c7",
        alpha = 0.4
      ) +
      annotate("rect",
        xmin = as.Date("2022-12-12"),
        xmax = as.Date("2022-12-24"),
        ymin = 0,
        ymax = Inf,
        fill = "#5ad45a",
        alpha = 0.4
      ) +
      geom_bar(just = 0) +
      geom_text(x = as.Date("2022-12-17"), y = 62, label = "Mesas", angle = 0, color = "#54504c") +
      geom_text(x = as.Date("2022-12-05"), y = 62, label = "Clases", angle = 0, color = "#54504c") +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        axis.text.x = element_text(size = rel(1.5)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      ggtitle("Cantidad de respuestas por día par toda la EII")
  })
  output$dias_deptos <- renderPlot({
    rtas %>%
      ggplot() +
      aes(x = `Fecha de inicio`) +
      annotate("rect",
        xmin = as.Date("2022-11-28"),
        xmax = as.Date("2022-12-12"),
        ymin = 0,
        ymax = Inf,
        fill = "#00b7c7",
        alpha = 0.4
      ) +
      annotate("rect",
        xmin = as.Date("2022-12-12"),
        xmax = as.Date("2022-12-24"),
        ymin = 0,
        ymax = Inf,
        fill = "#5ad45a",
        alpha = 0.4
      ) +
      geom_bar(just = 0) +
      facet_wrap(~Departamento, scales = "free_y") +
      geom_text(x = as.Date("2022-12-17"), y = 40, label = "Mesas", angle = 0, color = "#54504c") +
      geom_text(x = as.Date("2022-12-05"), y = 40, label = "Clases", angle = 0, color = "#54504c") +
      xlab("Fechas") +
      ylab("Cantidad de respuestas") +
      theme(
        axis.text.x = element_text(size = rel(1.5)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      ggtitle("Cantidad de respuestas por día para cada Departamento")
  })


  # Pestaña A
  output$CategoriaA <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA A: APRENDIZAJE</b>",
        "Mide si los estudiantes encontraron las clases estimulantes y sus intereses aumentaron al cursar la asignatura."
      ),
      "</p>"
    )
  })
  output$SubcategoriasA <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatA <- renderText({
    paste0(
      "<p>",
      c(
        "<b>A1-</b> La asignatura me pareció intelectualmente atractiva y estimulante.",
        "<b>A2-</b> Aprendí cosas que considero valiosas.",
        "<b>A3-</b> Aprendí y comprendí los contenidos de la asignatura."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatA <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatA_depto_elegido <- renderPlot({
    rtas %>%
      select(A1:A3, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIA <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$A_depto_elegido <- renderPlot({
    rtas %>%
      select(A1:A3, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$A_EII <- renderPlot({
    rtas %>%
      select(A1:A3, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosA <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$A_Deptos <- renderPlot({
    rtas %>%
      select(A1:A3, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })

  # Pestaña B
  output$CategoriaB <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA B: ENTUSIASMO</b>",
        "Evalúa la habilidad de los profesores para atraer y mantener la atención de los estudiantes durante la clase."
      ),
      "</p>"
    )
  })
  output$SubcategoriasB <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatB <- renderText({
    paste0(
      "<p>",
      c(
        "<b>B1-</b> Los docentes mostraron entusiasmo impartiendo la asignatura.",
        "<b>B2-</b> Los docentes han sido dinámicos y activos durante el cursado.",
        "<b>B3-</b> Los docentes lograron que el cursado resulte ameno.",
        "<b>B4-</b> Los docentes consiguieron mantener mi interés en la asignatura."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatB <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatB_depto_elegido <- renderPlot({
    rtas %>%
      select(B1:B4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIB <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$B_depto_elegido <- renderPlot({
    rtas %>%
      select(B1:B4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$B_EII <- renderPlot({
    rtas %>%
      select(B1:B4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosB <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$B_Deptos <- renderPlot({
    rtas %>%
      select(B1:B4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })

  # Pestaña C
  output$CategoriaC <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA C: ORGANIZACIÓN</b>",
        "Mide si el material y las explicaciones están alineados con los objetivos de la asignatura, y si se trabaja coordinadamente."
      ),
      "</p>"
    )
  })
  output$SubcategoriasC <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatC <- renderText({
    paste0(
      "<p>",
      c(
        "<b>C1-</b> Las explicaciones de los docentes han resultado claras.",
        "<b>C2-</b> El material de la asignatura estaba bien preparado y pude acceder a él sin dificultad.",
        "<b>C3-</b> Los docentes trabajaron coordinadamente."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatC <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatC_depto_elegido <- renderPlot({
    rtas %>%
      select(C1:C3, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = c("C1", "C2", "C3")) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIC <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$C_depto_elegido <- renderPlot({
    rtas %>%
      select(C1:C3, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = c("C1", "C2", "C3")) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$C_EII <- renderPlot({
    rtas %>%
      select(C1:C3, Departamento) %>%
      pivot_longer(cols = c("C1", "C2", "C3")) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosC <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$C_Deptos <- renderPlot({
    rtas %>%
      select(C1:C3, Departamento) %>%
      pivot_longer(cols = c("C1", "C2", "C3")) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })

  # Pestaña D
  output$CategoriaD <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA D: INTERACCIÓN CON EL GRUPO</b>",
        "Evalúa la habilidad de los profesores para animar a los estudiantes a participar de discusiones, hacer preguntas, expresar sus propias ideas y cuestionar las de los demás, durante la clase."
      ),
      "</p>"
    )
  })
  output$SubcategoriasD <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatD <- renderText({
    paste0(
      "<p>",
      c(
        "<b>D1-</b> Durante el cursado se estimulaba a los estudiantes a participar en las discusiones.",
        "<b>D2-</b> Durante el cursado se estimulaba a los estudiantes a realizar preguntas.",
        "<b>D3-</b> Durante el cursado se estimulaba a los estudiantes a expresar sus propias ideas, opiniones y conocimientos.",
        "<b>D4-</b> Durante el cursado se estimulaba a los estudiantes a cuestionar las ideas expresadas por los profesores y por los demás estudiantes."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatD <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatD_depto_elegido <- renderPlot({
    rtas %>%
      select(D1:D4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIID <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$D_depto_elegido <- renderPlot({
    rtas %>%
      select(D1:D4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$D_EII <- renderPlot({
    rtas %>%
      select(D1:D4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosD <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$D_Deptos <- renderPlot({
    rtas %>%
      select(D1:D4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })


  # Pestaña E
  output$CategoriaE <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA E: ACTITUD PERSONAL</b>",
        "Mide la habilidad del equipo docente para relacionarse individualmente con los alumnos, siendo amable, mostrando interés sincero, siendo accesible, ya sea durante la clase o después de ella."
      ),
      "</p>"
    )
  })
  output$SubcategoriasE <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatE <- renderText({
    paste0(
      "<p>",
      c(
        "<b>E1-</b> Los docentes se han mostrado amables con los estudiantes.",
        "<b>E2-</b> Los docentes han mostrado interés sincero por los estudiantes.",
        "<b>E3-</b> Los docentes se han mostrado accesibles en el trato individual con los estudiantes.",
        "<b>E4-</b> Los docentes hicieron sentir cómodos a los estudiantes que pidieron ayuda o consejos en clase y fuera de ella."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatE <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatE_depto_elegido <- renderPlot({
    rtas %>%
      select(E1:E4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIE <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$E_depto_elegido <- renderPlot({
    rtas %>%
      select(E1:E4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$E_EII <- renderPlot({
    rtas %>%
      select(E1:E4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosE <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$E_Deptos <- renderPlot({
    rtas %>%
      select(E1:E4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })


  # Pestaña F
  output$CategoriaF <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA F: CONTENIDO</b>",
        "Evalúa si el equipo docente presenta fundamentos, planteamientos alternativos, puntos de vista diferentes a los propios, avances y descubrimientos en la materia."
      ),
      "</p>"
    )
  })
  output$SubcategoriasF <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatF <- renderText({
    paste0(
      "<p>",
      c(
        "<b>F1-</b> Los docentes presentaron fundamentos de los conceptos desarrollados en la clase.",
        "<b>F2-</b> Los docentes expusieron otros puntos de vista diferentes del suyo cuando era apropiado.",
        "<b>F3-</b> Los docentes presentaron los avances y situación actuales en la materia.",
        "<b>F4-</b> Los docentes relacionaron conceptos con otros aprendidos con anterioridad."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatF <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatF_depto_elegido <- renderPlot({
    rtas %>%
      select(F1:F4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIF <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$F_depto_elegido <- renderPlot({
    rtas %>%
      select(F1:F4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$F_EII <- renderPlot({
    rtas %>%
      select(F1:F4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosF <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$F_Deptos <- renderPlot({
    rtas %>%
      select(F1:F4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })

  # Pestaña G
  output$CategoriaG <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA G: EVALUACIONES</b>",
        "Mide si el método de evaluación es justo, apropiado y alineado con los contenidos y si los docentes realizaron retroalimentaciones."
      ),
      "</p>"
    )
  })
  output$SubcategoriasG <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatG <- renderText({
    paste0(
      "<p>",
      c(
        "<b>G1-</b> Las devoluciones de los profesores sobre las evaluaciones fueron de gran ayuda.",
        "<b>G2-</b> Los métodos de evaluación que se implementaron fueron equitativos y adecuados.",
        "<b>G3-</b> Los contenidos de las evaluaciones se correspondieron con los contenidos del curso, y de acuerdo con el énfasis puesto en el cursado.",
        "<b>G4-</b> Los enunciados y consignas de las evaluaciones fueron claras."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatG <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatG_depto_elegido <- renderPlot({
    rtas %>%
      select(G1:G4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIG <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$G_depto_elegido <- renderPlot({
    rtas %>%
      select(G1:G4, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$G_EII <- renderPlot({
    rtas %>%
      select(G1:G4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosG <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$G_Deptos <- renderPlot({
    rtas %>%
      select(G1:G4, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })


  # Pestaña H
  output$CategoriaH <- renderText({
    paste0(
      "<p>",
      c(
        "<b>CATEGORÍA H: TRABAJOS Y MATERIALES</b>",
        "Evalúa la habilidad del equipo docente para proveer lecturas, materiales  y tareas (trabajos prácticos, de campo, laboratorios, talleres, presentaciones, etc.) que contribuyan a entender la asignatura."
      ),
      "</p>"
    )
  })
  output$SubcategoriasH <- renderText({
    paste("<b>Subcategorías</b>")
  })
  output$descr_subcatH <- renderText({
    paste0(
      "<p>",
      c(
        "<b>H1-</b> El material (bibliografía, apuntes, ejercitación, etc.) recomendado ha sido suficiente y adecuado.",
        "<b>H2-</b> Los recursos didácticos utilizados para explicar los temas fueron de gran ayuda.",
        "<b>H3-</b> Las tareas solicitadas (trabajos prácticos, presentaciones, proyectos, etc) contribuyeron a mejorar la valoración y la comprensión de la materia."
      ),
      "</p>"
    )
  })
  output$titulo_seccion_subcatH <- renderText({
    paste("Resumen de respuestas para cada <b>subcategoría</b> evaluada del Departamento de <b>", depto_react(), "</b>")
  })
  output$subcatH_depto_elegido <- renderPlot({
    rtas %>%
      select(H1:H3, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~name, nrow = 1) +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsEIIH <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados del Departamento con los de toda la EII)")
  })
  output$H_depto_elegido <- renderPlot({
    rtas %>%
      select(H1:H3, Departamento) %>%
      filter(Departamento == depto_react()) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para el Departamento",
        subtitle = depto_react()
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "none",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold"),
        plot.subtitle = element_text(size = rel(1.2), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$H_EII <- renderPlot({
    rtas %>%
      select(H1:H3, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      scale_x_discrete(drop = FALSE) +
      ggtitle(
        label = "Resultados para toda la EII",
        subtitle = ""
      ) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(1.3), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  output$titulo_seccion_DeptovsDeptosH <- renderText({
    paste("Resumen global de la <b>categoría</b> evaluada (comparación de resultados entre todos los Departamentos de la EII)")
  })
  output$H_Deptos <- renderPlot({
    rtas %>%
      select(H1:H3, Departamento) %>%
      pivot_longer(cols = -Departamento) %>%
      ggplot() +
      aes(
        x = factor(
          x = value,
          levels = escala_acuerdo,
          ordered = TRUE
        ),
        fill = value
      ) +
      geom_bar(width = 0.5) +
      facet_wrap(~Departamento, scales = "free_y") +
      scale_x_discrete(drop = FALSE) +
      xlab(NULL) +
      ylab("Cantidad de respuestas") +
      theme(
        legend.position = "right",
        legend.title = element_text(size = rel(1.3), face = "bold"),
        axis.text.x = element_text(size = rel(1)),
        axis.ticks = element_line(0),
        legend.text = element_text(size = rel(1.2)),
        strip.text.x = element_text(size = rel(1.5), face = "bold")
      ) +
      scale_fill_manual(
        drop = FALSE,
        values = c(
          "En total desacuerdo" = "#66c2a4",
          "En desacuerdo" = "#fc8e62",
          "Neutro" = "#8ea0cb",
          "De acuerdo" = "#e78bc3",
          "Completamente de acuerdo" = "#a6d753",
          "No contesta" = "#d0d8f8"
        ),
        na.value = "#d0d8f8",
        name = "Grado de acuerdo",
        breaks = escala_acuerdo,
        labels = c(
          "1- En total desacuerdo",
          "2- En desacuerdo",
          "3- Neutro",
          "4- De acuerdo",
          "5- Completamente de acuerdo",
          "No contesta"
        )
      ) +
      guides(x = guide_axis(n.dodge = 2))
  })
  
  # Pestaña Información General
  output$info_gral <- renderText({
    paste0(
      "<p>",
      c("Esta aplicación está diseñada para resumir y presentar la información global que se desprende de la encuesta de fin de cursado que implementa la <b>Escuela de Ingeniería Industrial (EII)</b> de la Facultad de Ciencias Exactas, Ingeniería y Agrimensura de la UNR.",
        "Está pensada para los siguientes usuarios:",
        "* Autoridades de la EII (Director/a y Secretario/a Académico/a)",
        "* Directores/as de Departamento de la EII",
        "<i>Los equipos docentes reciben la información en otro formato y con diferente grado de detalle que la propuesta para los destinatarios de esta aplicación.</i>",
        "Las fuentes de los datos son dos:",
        "* El módulo de gestión de encuestas Kolla, del Sistema de Información Universitaria (SIU), que cuatrimestralmente recopila las respuestas de los/as estudiantes sobre el cursado de las asignaturas a las que se inscribieron. Los datos son recibidos por la EII en un archivo de texto plano (el archivo correspondiente a la última encuesta es el llamado <b>2022_02.txt</b>).  Este archivo tiene en las columnas 8 a 36 las respuestas de cada estudiante que respondió. En las otras columnas hay información de la actividad curricular, fechas de respuesta y otras variables que no son de interés para el fin de esta aplicación.",
        "* Una base de datos de la EII desde la que se genera un archivo de texto plano con información de las actividades curriculares de la Escuela (nombre de la actividad curricular, Departamento al cual pertenece y cuatrimestre correspondiente del plan de estudio de la carrera). El archivo es llamado <b>asignaturas.txt</b>.",
        "Para limpiar y ordenar los datos se procedió a:",
        "* Cambiar los nombres de las columnas 8 a 36 por el código interno de la pregunta y que sirve de referencia para armar las visualizaciones y para comparar con encuestas de cuatrimestres anteriores.",
        "* Rellenar con “No contesta” los datos faltantes (NA).",
        "* Transformar en factor las variables de interés (columnas 8 a 36)",
        "* Fusionar los <i>datasets</i> para poder generar análisis por Departamento."
        ),
      "</p>"
    )
  })
}


# 5- Ejecución de la aplicación
shinyApp(ui = MiInterfaz, server = MiServidor)
