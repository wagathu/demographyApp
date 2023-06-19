library(shiny)
library(shinyWidgets)
library(slickR)
library(shinyjs)
library(scales)
source("Analysis.R")
source("chloropleth.R")

ui <- fluidPage(
  tabsetPanel(type = "pills",
              tabPanel("Welcome",
                       fluidRow(
                         leaflet() %>% 
                           addProviderTiles("OpenStreetMap.Mapnik") %>% 
                           addPolygons(data = data4,
                                       opacity = 0,
                                       color = "red")
                         ,
                         br(),
                         hr(),
                         sidebarLayout(
                           sidebarPanel(
                         panel(status = "danger",
                               position = "center",
                               h2("Welcome to the Kenya Demographic Analysis App!"),
                               p("Explore the demographic characteristics of Kenya and gain insights into key factors shaping its population."),
                               p("This interactive app provides visualizations and analysis of various demographic indicators such as population size, fertility rate, mortality rate, and more."),
                               p("Use the sidebar controls to customize the data and parameters you want to analyze. The app dynamically updates the visualizations based on your selections."),
                               p("Gain a deeper understanding of Kenya's population dynamics and observe trends and patterns over time."),
                               p("Get ready to dive into the fascinating world of Kenya's demographics! Start exploring now.")
                         )
                         ), mainPanel(   slickROutput("slickr")
                           
                         )
                         )
                       )
              ),
              tabPanel("Brief History of Kenya",
                       br(),
                       panel(status = "primary", position = "center",
                             h2("History of Kenya"),
                             h3("Pre-Colonial History"),
                             p(strong("The region that is now known as Kenya has a rich and diverse pre-colonial history that spans thousands of years. Around 2000 BC, Cushitic-speaking people from northern Africa settled in what is now Kenya. These early inhabitants engaged in hunting, fishing, and gathering, and they established communities along the coast and in the fertile highlands. Over time, the Kenyan coast became a hub for trade with Arab merchants, who brought influences from the Arabian Peninsula and established Arab and Persian colonies along the coast.")),
                             p(strong("During the first millennium AD, the Nilotic and Bantu people migrated into the region from different parts of Africa. The Nilotic groups, including the Maasai and Samburu, settled in the arid and semi-arid areas of Kenya, while the Bantu people, such as the Kikuyu, Luo, and Luhya, moved into the fertile highlands and settled inland. The Bantu communities practiced agriculture, cultivating crops such as millet, sorghum, and later maize, which became a staple food in the region. Over time, a unique blend of Bantu and Arabic cultures emerged, giving rise to the Swahili culture and language.")),
                             h3("Colonial History"),
                             p(strong("Kenya's colonial history began in the late 19th century when European powers, particularly Britain, sought to expand their influence and control in Africa. In 1885, the Berlin Conference assigned the region to the British East Africa Company, which established the British East Africa Protectorate in 1895. The British saw the region as a source of valuable resources and a potential route to the interior of Africa.")),
                             p(strong("The British colonial administration in Kenya focused on exploiting the country's resources and establishing large-scale agriculture, particularly in the fertile highlands. The colonial authorities introduced cash crops such as coffee, tea, and sisal, which transformed Kenya into a major exporter of agricultural products. The indigenous African population, however, faced dispossession of their lands and limited political representation. Settler communities from Europe, mainly from Britain, were encouraged to migrate to Kenya and were granted large tracts of land.")),
                             h3("Struggles for Independence"),
                             p(strong("In the early 20th century, the African population in Kenya began organizing and advocating for their rights and independence. Nationalist movements, such as the East Africa Association and the Kenya African Union (KAU), emerged to challenge colonial rule and demand political representation and land rights.")),
                             p(strong("The struggle for independence intensified in the 1950s with the formation of the Kenya African National Union (KANU) and the rise of influential leaders such as Jomo Kenyatta. The Mau Mau uprising, a militant anti-colonial movement primarily led by the Kikuyu, sought to reclaim land and challenge British domination. The conflict between the Mau Mau fighters and the British colonial forces was marked by violence and repression, but it also galvanized international support for Kenya's independence.")),
                             h3("Independence and Post-Colonial Era"),
                             p(strong("Kenya gained independence from British colonial rule on December 12, 1963. Jomo Kenyatta, a prominent nationalist leader and founding father of the nation, became Kenya's first President. The early years of independence were focused on nation-building, economic development, and social progress.")),
                             p(strong("The Kenyan government implemented various policies and programs to promote education, healthcare, and infrastructure development. The country embarked on an ambitious plan known as the Sessional Paper No. 10 of 1965, which aimed to achieve rapid industrialization and reduce dependency on agriculture. Kenya also pursued a policy of non-alignment in international affairs, seeking partnerships with both Western and Eastern bloc countries.")),
                             h3("Challenges and Progress"),
                             p(strong("Since independence, Kenya has faced various challenges and made significant progress in different areas. The country has experienced political transitions, with successive leaders steering the nation through periods of stability and transformation. However, Kenya has also faced issues such as political corruption, ethnic tensions, and unequal distribution of resources, which have posed obstacles to its development.")),
                             p(strong("In recent decades, Kenya has focused on promoting democratic governance, improving infrastructure, and diversifying its economy. The country has become a regional hub for business and innovation, particularly in sectors such as technology and telecommunications. Kenya's wildlife reserves, national parks, and cultural heritage have made it a popular tourist destination, contributing to its economic growth.")),
                             h3("Towards a Bright Future"),
                             p(strong("Kenya continues to strive for social progress, economic prosperity, and national unity. The country's youth, entrepreneurship, and vibrant civil society play a crucial role in shaping its future. Kenya's commitment to sustainable development, environmental conservation, and inclusive growth positions it as a dynamic nation in the African context.")),
                             p(strong("Understanding the history of Kenya provides valuable insights into its cultural heritage, social dynamics, and political landscape. It is a testament to the resilience, diversity, and aspirations of the Kenyan people. Explore the fascinating journey of Kenya and its people to gain a deeper appreciation of this vibrant nation. Video Credits."))
                       )),
              
              tabPanel("Population Per County",
                       br(),
                       sidebarLayout(
                         sidebarPanel(
                           status = "primary",
                           position = "left",
                           h3("Introduction"),
                           p(
                             strong("There are 47 Counties in Kenya. Each county has its own unique characteristics, shaped by different geographical and economic factors. These counties play a crucial role in the governance and development of the country. Let's explore some key aspects of the Kenya counties.")
                           ),
                           
                           h3("Population"),
                           p(
                             strong("The 47 counties in Kenya exhibit significant variations in population size. Some counties have larger populations, while others are relatively smaller. These variations can be attributed to factors such as historical settlement patterns, urbanization, economic opportunities, and natural resources. Nairobi County, the capital city, stands out as the most populous county in Kenya, serving as the country's economic and administrative hub. On the other hand, some counties, such as Lamu County or Tana River County, have smaller populations due to factors like their remote locations or limited economic activities.")
                           ),
                           
                           h3("Geographical Factors"),
                           p(
                             strong("The counties in Kenya showcase diverse geographical features. From the snow-capped peaks of Mount Kenya in Nyeri County to the vast plains of the Maasai Mara in Narok County, each county has its own distinct landscapes. Some counties are characterized by fertile agricultural lands, such as Nakuru County, which is known for its thriving farming activities. Other counties, like Turkana County, have arid and semi-arid regions, presenting unique challenges and opportunities related to water scarcity and pastoralism.")
                           ),
                           
                           h3("Economic Factors"),
                           p(
                             strong("Economic activities vary across the counties in Kenya, leading to disparities in development and wealth distribution. Certain counties have vibrant economies driven by sectors such as finance, commerce, and tourism. For instance, Mombasa County, with its strategic coastal location, serves as a major international trade and tourism hub. At the same time, counties like Kisumu County benefit from their proximity to Lake Victoria and their agricultural productivity. However, some counties face economic challenges, including limited infrastructure, unemployment, and poverty. Efforts are being made to promote economic growth and reduce these disparities through various initiatives and investments.")
                           ),
                           
                           h3("Largest and Smallest Counties"),
                           p(
                             strong("Among the 47 counties, some stand out as the largest and smallest in terms of land area. Turkana County, located in the northwestern part of Kenya, is the largest county, known for its expansive landscapes and rich cultural heritage. On the other hand, Mombasa County, with its coastal strip, is the smallest county in terms of land area. Despite their differences in size, each county contributes to the cultural, social, and economic tapestry of Kenya.")
                           ),
                           p(strong("The map on the Right Hand Side shows the total population of each county as well as population per gender of each county."))
                           
                         ),
                         
                         mainPanel(panel(status = "primary",
                                         p(strong("Hover or Touch to Explore County Population Demographics")),
                                         leafletOutput("maping", width = "100%", height = "1090px")))
                         
                       ),
                       (
                         panel(status = "primary", position ="center",
                               heading = h2(strong("Population of the counties According to Gender")),  
                               sidebarLayout(
                                 sidebarPanel(width = 3,
                                              selectInput(
                                                inputId = "counties",
                                                label = "Select your demographic variable", 
                                                choices = names(counties[, 2:4])  
                                              ),
                                              br(),
                                              br()
                                 ),
                                 
                                 mainPanel(p(strong("This plot illustrates the population of each gender per county")),
                                           plotlyOutput("pl1")))))
                         
                       ),
              tabPanel("Kenya Population Demographics",
                       fluidRow(
                         (p(h4(("The Population of Kenya as well as other population charachteristics
                                       have been changing as shown in the plots below")))),
                         column(6,
                                br(),
                                panel(heading = p(strong("Population")), status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "pop","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p2")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Fertility Rate")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Fertility","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p4")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Life Expectancy")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Life","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p6")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Per Capita Income")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Capita","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p8")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Unemployment Rate")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Unemployment","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p10")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("CO2 Emmision")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "CO","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p12"))))
                                
                                
                         ),
                         
                         column(6,
                                br(),
                                panel(heading = p(strong("Population Growth Rate")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "growth","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p3")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Mortality Rate")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Mortality","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p5")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Gross Domestic Product")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Gdp","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p7")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("Infant Mortality Rate")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Infant","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p9")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("% of GDP spent on Education")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Education","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p11")))),
                                br(),
                                hr(),
                                br(),
                                panel(heading = p(strong("% of Population with Electricity Access")),status = "primary",
                                      sidebarLayout(
                                        sidebarPanel(
                                          actionButton( "Electricity","Generate Plot", class = "btn-success")
                                          
                                        ),
                                        mainPanel(plotlyOutput("p13"))))
                                
                         )
                       )
              )
              
  )
)

# splitLayout(cellWidths = c("50%", "50%"),
#             panel(status = "primary",
#                   position = "left",
#                   slickROutput("slickr")
#             ),
#             panel(status = "primary",
#                   position = "right",
#                   plotlyOutput("CountyPop")
#             )
# )

server <- function(input, output) {
  output$slickr <- renderSlickR({
    
    slickR(imgs,
           height = "300px",
           width = "3300px",
    )+ settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 2000)
  })
  
  output$CountyPop <- renderPlotly({
    p1 <- counties |>
      ggplot(aes(x = County)) + 
      geom_col(aes(y = Total), fill = "royalblue") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks = element_line(colour = "royalblue", size = 1),
            axis.line = element_line(colour = "royalblue")) +
      labs(title = "Kenya Population by Counties") +
      xlab("County") +
      ylab("Head Count")
    
    ggplotly(p1, tooltip = c("County"))
    
  })
  output$maping <- renderLeaflet({
    labels <- sprintf(
      "<strong>%s</strong><br/>Male Population: %s<br/>Female Population: %s<br/>Intersex Population: %s<br/>Total Population: %s",
      data4$county,
      format(data4$Male, big.mark = ","),
      format(data4$Female, big.mark = ","),
      format(data4$Intersex, big.mark = ","),
      format(data4$Total, big.mark = ",")
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorNumeric(palette = "Blues", domain = data3$Total)
    
    leaflet() %>%
      setView(lng = 37.2, lat = -0.4864, zoom = 7) %>%

      addPolygons(
        data = data4,
        fillColor = ~pal(Total),
        fillOpacity = 1,
        color = "red",
        weight = 1,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "bold", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto",
          offset = c(0, 20),
          opacity = 0.77,
          color = "black"
        ),
        highlightOptions = highlightOptions(
          weight = 5,
          fillOpacity = 0.7,
          color = "red",
          bringToFront = TRUE,
          stroke = TRUE
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = data4$Total, title = "Population", opacity = 0.7)
  })
  
    output$pl1 <- renderPlotly({
      pl1 <- counties |> 
        ggplot(aes(x = County)) +
        geom_col(aes(y = .data[[input$counties]]), fill = "navyblue") +
        theme_minimal() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks = element_line(colour = "navyblue", size = 1),
              axis.line = element_line(colour = "navyblue")) +
        labs(title = "Kenya Population by Counties") +
        ylab(paste(input$counties, "Population"))+
        scale_y_continuous(labels = label_number_si())
      
      ggplotly(pl1, tooltip = c("County","y"), label = label_number_si())
      
    })
    observeEvent(input$pop,{
      
      output$p2 <- renderPlotly({
        p2 <- population |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = Population), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "Kenya Population Since 1950") +
          xlab("Year") +
          ylab("Head Count") +
          scale_y_continuous(labels = label_number_si())
        ggplotly(p2)
      })
    })
    
    observeEvent(input$growth,{
      output$p3 <- renderPlotly({
        p3 <- population |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Growth Rate`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1)) +
          labs(title = "Kenya Population Growth Rate Since 1950") +
          xlab("Year") +
          ylab("Growth Rate in %")
        ggplotly(p3)
        
      })
    })
    
    observeEvent(input$Fertility, {
      output$p4 <- renderPlotly({
        
        p4 <- Fertility |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Fertility Rate`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1)) +
          labs(title = "Kenya Ferility Rate Since 1950") +
          xlab("Year") +
          ylab("Ferility Rate in %")
        ggplotly(p4)
        
      })  
      
    })
    
    observeEvent(input$Mortality,{
      output$p5 <- renderPlotly({
        p5 <- Mortality |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Death Rate`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "Kenya Mortality Rate Since 1950") +
          xlab("Year") +
          ylab("Mortality Rate in %")
        ggplotly(p5)
      })
      
      
    })
    
    observeEvent(input$Life,{
      output$p6 <- renderPlotly({
        p6 <- Expectancy |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Life Expectancy`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1)) +
          labs(title = "Life Expectnacy Since 1950") +
          xlab("Year") +
          ylab("Life Expectancy in %")
        ggplotly(p6)
      })
    })
    
    observeEvent(input$Gdp,{
      output$p7 <- renderPlotly({
        p7 <- Gdp |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `GDP(Billions)`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "GDP(Billions) Since 1960") +
          xlab("Year") +
          ylab("GDP(Billions)")
        ggplotly(p7)
        
      })
      
    })
    
    observeEvent(input$Capita,{
      output$p8 <- renderPlotly({
        p8 <- Gdp |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Per Capita`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "Per Capita income Since 1960") +
          xlab("Year") +
          ylab("Per Capita")
        ggplotly(p8)
        
      })
    })
    
    observeEvent(input$Infant,{
      output$p9 <- renderPlotly({
        p9 <- infantM |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Infant Mortality Rate`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "Infant Mortality Since 1950") +
          xlab("Year") +
          ylab("Infant Mortality")
        ggplotly(p9)
      })
    })
    
    observeEvent(input$Unemployment,{
      output$p10 <- renderPlotly({
        p10 <- Unemployment |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Unemployment Rate (%)`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "Unemployment Rate Since 1991") +
          xlab("Year") +
          ylab("Unemployment Rate in %")
        ggplotly(p10)
      })
    })
    
    observeEvent(input$Education,{
      output$p11 <- renderPlotly({
        p11 <- Education |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Education Spending (% of GDP)`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "Education Spending (% of GDP) Since 1982") +
          xlab("Year") +
          ylab("Spending")
        ggplotly(p11)
        
      })
    })
    
    observeEvent(input$CO,{
      output$p12 <- renderPlotly({
        p12 <- Co2 |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `Kilotons of Co2`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "CO2 Emmision(in Kilotons) In Kenya Since 1960") +
          xlab("Year") +
          ylab("CO2")
        ggplotly(p12)
      })
    })
    
    observeEvent(input$Electricity,{
      output$p13 <- renderPlotly({
        p13 <- Electricity |>
          ggplot(aes(x = Year)) + 
          geom_line(aes(y = `% of Population`), col = "royalblue", size = .7) +
          theme_minimal() +
          theme(axis.text = element_text(colour = "royalblue"),
                axis.title = element_text(color = "royalblue"),
                axis.line = element_line(colour = "royalblue"),
                plot.title = element_text(colour = "royalblue", hjust = .5),
                axis.ticks = element_line(colour = "royalblue", linewidth = 1))+
          labs(title = "% of Population with Electricity Access In Kenya Since 1993") +
          xlab("Year") +
          ylab("CO2")
        ggplotly(p13)
      })
    })
  
}

shinyApp(ui, server)
