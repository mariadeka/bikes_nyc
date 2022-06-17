library("shiny")
library("dplyr")
library("shinythemes")

jan <- read.csv("~/Desktop/studia/2.semestr/PDU/201501-citibike-tripdata.csv")
feb <- read.csv("~/Desktop/studia/2.semestr/PDU/201502-citibike-tripdata.csv")
mar <- read.csv("~/Desktop/studia/2.semestr/PDU/201503-citibike-tripdata.csv")
apr <- read.csv("~/Desktop/studia/2.semestr/PDU/201504-citibike-tripdata.csv")
mai <- read.csv("~/Desktop/studia/2.semestr/PDU/201505-citibike-tripdata.csv")
jun <- read.csv("~/Desktop/studia/2.semestr/PDU/201506-citibike-tripdata.csv")
jul <- read.csv("~/Desktop/studia/2.semestr/PDU/201507-citibike-tripdata.csv")
aug <- read.csv("~/Desktop/studia/2.semestr/PDU/201508-citibike-tripdata.csv")
sep <- read.csv("~/Desktop/studia/2.semestr/PDU/201509-citibike-tripdata.csv")
oct <- read.csv("~/Desktop/studia/2.semestr/PDU/201510-citibike-tripdata.csv")
nov <- read.csv("~/Desktop/studia/2.semestr/PDU/201511-citibike-tripdata.csv")
dec <- read.csv("~/Desktop/studia/2.semestr/PDU/201512-citibike-tripdata.csv")
dane <- jan%>%full_join(feb)%>%full_join(mar)%>%full_join(apr)%>%
  full_join(mai)%>%full_join(jun)%>%full_join(jul)%>%full_join(aug)%>%
  full_join(sep)%>%full_join(oct)%>%full_join(nov)%>%full_join(dec)
dane$starttime <- format(as.POSIXct(dane$starttime, format="%m/%d/%Y %H:%M"), "%m")
profil <- function(dane){
  prof <- dane%>%filter(gender == 1| gender == 2)%>%
    select(birth.year, usertype, gender)%>%
    mutate(birth.year = 2015 - birth.year)%>%
    mutate(gender = case_when(
      gender == 1 ~ "mężczyzna",
      gender == 2 ~ "kobieta"))
  prof <- rename(prof, age = birth.year)
  return(prof)
}
user_prof <- profil(dane)

time_age <- function(dane){
  dane$starttime <- format(as.POSIXct(dane$starttime, format="%m/%d/%Y %H:%M"), "%H")
  t_age <- dane%>%select(birth.year, starttime)%>%mutate(
    birth.year = 2015 - birth.year)%>%
    mutate(birth.year = case_when(
      birth.year >= 60 ~ "senior",
      birth.year <= 26 ~ "uczeń",
      TRUE ~ "dorosły"))
  t_age <- rename(t_age, age = birth.year)
  return(t_age)
}
ui6 <- fluidPage(
  titlePanel("OGÓLNE INFORMACJE"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("choice",
                   "co chcesz sprawdzić",
                   choices = c("ilość wypożyczeń",
                               "stosunek liczby kilentów do liczby subskrybentów",
                               "stosunek ilości kobiet do mężczyzn",
                               "pory wypożyczania rowerów",
                               "rozkład wieku użytkowników")),
      conditionalPanel(condition = "input.choice == 'stosunek liczby kilentów do liczby subskrybentów'",
                       selectInput(inputId = "month",
                                   label = "który miesiąc Cie interesuje",
                                   choices = c("styczeń", "luty", "marzec", "kwiecień", "maj",
                                               "czerwiec", "lipiec", "sierpień", "wrzesień",
                                               "październik", "listopad", "grudzień")),
                       ),
      conditionalPanel(condition = "input.choice == 'pory wypożyczania rowerów'",
                       selectInput(inputId = "grupa_wiekowa",
                                   label = "wybierz grupe wiekowa",
                                   choices = c("uczeń", "dorosły", "senior")),
                       selectInput(inputId = "miesiac",
                                   label = "wybierz interesujący Cię miesiąc:",
                                   choices = c("styczeń", "luty", "marzec", "kwiecień", "maj",
                                               "czerwiec", "lipiec", "sierpień", "wrzesień",
                                               "październik", "listopad", "grudzień"))
                       ),
      ),
    mainPanel(
      plotOutput(outputId = "wyjscieHistogram")
      )
    )
  )
server6 <- function(input, output){
  output$wyjscieHistogram <- renderPlot({
    if(input$choice == "stosunek liczby kilentów do liczby subskrybentów"){
      if(input$month == "styczeń"){
        liczba <- table(jan$usertype)
        pie(liczba)
        } else if(input$month == "luty"){
        liczba <- table(feb$usertype)
        pie(liczba)
        } else if(input$month == "marzec"){
        liczba <- table(mar$usertype)
        pie(liczba)
        } else if(input$month == "kwiecień"){
        liczba <- table(apr$usertype)
        pie(liczba)
        } else if(input$month == "maj"){
        liczba <- table(mai$usertype)
        pie(liczba)
        } else if(input$month == "czerwiec"){
        liczba <- table(jun$usertype)
        pie(liczba)
        } else if(input$month == "lipiec"){
        liczba <- table(jul$usertype)
        pie(liczba)
        } else if(input$month == "sierpień"){
        liczba <- table(aug$usertype)
        pie(liczba)
        } else if(input$month == "wrzesień"){
        liczba <- table(sep$usertype)
        pie(liczba)
        } else if(input$month == "październik"){
        liczba <- table(oct$usertype)
        pie(liczba)
        } else if(input$month == "listopad"){
        liczba <- table(nov$usertype)
        pie(liczba)
        } else {
        liczba <- table(dec$usertype)
        pie(liczba)
        }
    } else if(input$choice == "ilość wypożyczeń"){
      liczba <- table(dane$starttime)
      barplot(liczba, xlab = "miesiąc", ylab = "ilość wypożyczeń")
      } else if(input$choice == "stosunek ilości kobiet do mężczyzn"){
        barplot(table(user_prof$gender))
        } else if(input$choice == "pory wypożyczania rowerów"){
          if(input$miesiac == "styczeń"){
            jan <- time_age(jan)
            dataf <- jan[jan$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "luty"){
            feb <- time_age(feb)
            dataf <- feb[feb$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "marzec"){
            mar <- time_age(mar)
            dataf <- mar[mar$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "kwiecień"){
            apr <- time_age(apr)
            dataf <- apr[apr$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "maj"){
            mai <- time_age(mai)
            dataf <- mai[mai$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "czerwiec"){
            jun <- time_age(jun)
            dataf <- jun[jun$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "lipiec"){
            jul <- time_age(jul)
            dataf <- jul[jul$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "sierpień"){
            aug <- time_age(aug)
            dataf <- aug[aug$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "wrzesień"){
            sep <- time_age(sep)
            dataf <- sep[sep$age == input$grupa_wiekowa, ]
            liczba <- table(dataf$starttime)
            barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "październik"){
              oct <- time_age(oct)
              dataf <- oct[oct$age == input$grupa_wiekowa, ]
              liczba <- table(dataf$starttime)
              barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "listopad"){
              nov <- time_age(nov)
              dataf <- nov[nov$age == input$grupa_wiekowa, ]
              liczba <- table(dataf$starttime)
              barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            } else if(input$miesiac == "grudzień"){
              dec <- time_age(dec)
              dataf <- dec[dec$age == input$grupa_wiekowa, ]
              liczba <- table(dataf$starttime)
              barplot(liczba, xlab = "godzina", ylab = "ilość wypożyczeń")
            }
        } else {
          barplot(table(user_prof$age), xlab = "wiek", ylab = "ilość wypożyczeń")

        }
    


            
  })
  }
shinyApp(ui6, server6)

