library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("LIFELOG 모니터링"),
  
  HTML("<div style='padding: 5px;'><p>버전: 2.0 (2024.01.28.)
        <br>이번 버전부터는 한 번에 여러개의 csv 파일을 올릴 수 있습니다. 파일 이름 변경과 종류별 중복 업로드는 지원되지 않습니다.</p></div>"),
  
  selectInput("location", "조회하고 싶은 기관을 고르시오:", 
              choices = c("전체", "서울대병원 & 관악보건진료소", "서울아산병원", "고려대안암병원", "울산과학기술원"),
              width = "100%"),
  
  fileInput("csvs", "CSV 파일을 업로드", 
            accept = ".csv", 
            multiple = TRUE, width = "100%"),
    
  tableOutput("resultsTable"),
  
  downloadButton("downloadData", "결과표 CSV로 다운로드")
)

# 초기 데이터 타입 객체 세팅
initialize_data_type_objects <- function() {
  type_labels <- c("Run-in period", "Daily report", "Usage", "GPS", "Call", "SMS", "Step", "Activity")
  setNames(replicate(8, tibble(DataType = NA, Period = NA, Summary = NA, Obs = NA), simplify = FALSE), type_labels)
}
 
# 파일 데이터 처리 및 요약 정보 업데이트
update_data_type_object <- function(dtype, file_names, data_types, type_labels, input, location_mapping) {
  if (length(file_names) == 0) {
    return(tibble(DataType = type_labels[which(data_types == dtype)], 
                  Period = NA, 
                  Summary = "No input", 
                  Obs = NA))
  }
  
  data_frames <- lapply(file_names, function(file_name) {
    parts <- unlist(strsplit(file_name, "_"))
    start_date <- str_sub(parts[1], 5, 8)
    end_date <- str_sub(parts[2], 5, 8)
    file_path <- input$csvs$datapath[which(input$csvs$name == file_name)]
    data <- read.csv(file_path, header = TRUE) %>%
      {colnames(.) <- c("device", "ID", "initial"); .} %>% 
      filter(str_sub(ID, 6, 6) %in% c("0", "1")) %>%
      filter(if(input$location != "전체") str_sub(ID, 7, 7) %in% unlist(location_mapping[input$location]) else TRUE)
    
    num_obs <- nrow(data)
    data_summary <- if (num_obs == 0) " " else paste(data$ID, "(", data$initial, ")", sep = "", collapse = " ")
    
    tibble(
      DataType = type_labels[which(data_types == dtype)],
      Period = paste(start_date, end_date, sep = " ~ "),
      Summary = data_summary,
      Obs = num_obs
    )
  })
  
  bind_rows(data_frames)
}



# server 함수
server <- function(input, output) {
  # Reactive 값으로 데이터 저장
  table_data <- reactive({
    req(input$csvs) # 파일 업로드 확인
    
    # 위치 매핑 및 기타 필요한 설정
    location_mapping <- list(
      "서울대병원 & 관악보건진료소" = c("1", "5"),
      "서울아산병원" = "2",
      "고려대안암병원" = "3",
      "울산과학기술원" = "6"
    )
    
    data_types <- c("run", "daily", "usage", "gps", "call", "sms", "step", "activity")
    type_labels <- c("Run-in period", "Daily report", "Usage", "GPS", "Call", "SMS", "Step", "Activity")
    data_objects <- initialize_data_type_objects()
    
    # CSV 파일 처리 및 객체 업데이트
    for (dtype in data_types) {
      file_names <- input$csvs$name[grepl(dtype, input$csvs$name)]
      data_objects[[type_labels[which(data_types == dtype)]]] <- update_data_type_object(dtype, file_names, data_types, type_labels, input, location_mapping)
    }
    
    # 모든 데이터 타입 객체 결합
    do.call(rbind, data_objects)
  })
  
  # 테이블 렌더링
  output$resultsTable <- renderTable({
    table_data()  # Reactive 값을 사용
  })
  
  # CSV 파일 다운로드
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("lifelog_monitoring_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(table_data(), file, row.names = FALSE)
    }
  )
}



# Run the application
shinyApp(ui = ui, server = server)
