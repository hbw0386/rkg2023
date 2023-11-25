library(ggplot2)
library(dplyr)
library(DT)
library(chron)
a23fgb=read.csv("a23rkgb.csv")
a23.fg=read.csv("a23.rkg.csv")
fangels=c("琳妲","苡萱","嘎琳","小紫","阿布舞","宋宋","筠熹","儷軒","語芯","卉妮",
          "陳伊","穎樂","孟潔","Yuri","熊霓","Kira","菲菲","心韻","雅涵","慧慧",
          "若潼","李昀","禹菡","凱莉絲","籃籃","十元","岱縈","曲曲","李多慧",
          "芷軒","凱伊","林襄","溫妮")
zzz=read.csv("zzz.csv")

angels.info1<-function(){
  a23.fg->x
  a=nrow(x)
  b=0.483
  c=round(mean(x$HomeScore),2)
  e=round(mean(x$VisitingScore),2)
  d=round(mean(x$GameTime),1)
  f=round(mean(x$AudienceCnt),1)
  z=((times(x$EndTime))>times("22:00:00"))
  cat("2023樂天桃猿主場紀錄","\n","\n")
  cat("Rakuten Girls:","全場次","\n")
  cat("出席場次:    ",a,"場","\n")
  cat("主場勝率:    ",b,"\n")
  cat("場均得分:    ",c,"\n")
  cat("場均失分:    ",e,"\n")
  cat("平均觀眾數： ",f,"\n")
  cat("平均比賽時間:",d,"分鐘","\n")
  cat("比賽>10pm結束:",length(z[z==TRUE]),"場","\n")
  cat("比賽均溫:     ",round(mean(x$Temperature,na.rm=TRUE),1),"度")
}
angels.info2<-function(q){
  a23.fg%>%filter(grepl(q,Angels))->x
  a=nrow(x)
  b=round(zzz$win[zzz$Name==q],3)
  c=round(mean(x$HomeScore),2)
  e=round(mean(x$VisitingScore),2)
  d=round(mean(x$GameTime),1)
  f=round(mean(x$AudienceCnt),1)
  z=((times(x$EndTime))>times("22:00:00"))
  cat("2023樂天桃猿主場紀錄","\n","\n")
  cat("Rakuten Girls:",q,"\n")
  cat("出席場次:    ",a,"場","\n")
  cat("主場勝率:    ",b,"\n")
  cat("場均得分:    ",c,"\n")
  cat("場均失分:    ",e,"\n")
  cat("平均觀眾數： ",f,"\n")
  cat("平均比賽時間:",d,"分鐘","\n")
  cat("比賽>10pm結束:",length(z[z==TRUE]),"場","\n")
  cat("比賽均溫:     ",round(mean(x$Temperature,na.rm=TRUE),1),"度")
}

angels.info4<-function(q){
  x=a23fgb
  y=a23.fg
  y%>%filter(grepl(q,Angels))->y
  x%>%filter(grepl(q,Angels))->x
  x%>%#group_by(HitterName)%>%
    summarize(G=n(),PA=sum(PlateAppearances,na.rm=TRUE), AB=sum(HitCnt,na.rm=TRUE), 
              H=sum(HittingCnt,na.rm=TRUE), IBB=sum(IntentionalBasesONBallsCnt,na.rm=TRUE), 
              BB=sum(BasesONBallsCnt,na.rm=TRUE), HBP=sum(HitBYPitchCnt,na.rm=TRUE),
              SF=sum(SacrificeFlyCnt,na.rm=TRUE), BA=H/AB, OBP=(H+BB+HBP)/(AB+BB+HBP+SF),
              SLG=sum(TotalBases,na.rm=TRUE)/AB, OPS=OBP+SLG, HR=sum(HomeRunCnt),
              MVP=sum(IsMvp,na.rm=TRUE), SB=sum(StealBaseOKCnt,na.rm=TRUE))->x
  x=x[c("G","PA","BB","H","HR","BA","SB","OBP","SLG","OPS","MVP")]
  x%>%arrange(desc(OPS))->x
  x$BA%>%round(3)%>%format(nsmall=3)->x$BA
  x$OBP%>%round(3)%>%format(nsmall=3)->x$OBP
  x$SLG%>%round(3)%>%format(nsmall=3)->x$SLG
  x$OPS%>%round(3)%>%format(nsmall=3)->x$OPS
  y%>%group_by(MVPname)%>%summarise(n=n())%>%arrange(desc(n))->z
  
  cat("2023樂天桃猿主場紀錄","\n","\n")
  cat("Rakuten Girls:",q,"\n")
  cat("出席場數:    ",nrow(y%>%filter(grepl(q,Angels))),"場","\n")
  cat("團隊打擊三圍:",x$BA,"/",x$OBP,"/",x$SLG,"\n")
  cat("團隊全壘打數:",x$HR,"\n")
  cat("團隊盜壘數:  ",x$SB,"\n")
  cat("團隊OPS:     ",x$OPS,"\n")
  #cat("獲得最多次MVP的選手:",max(z$n),"次,",z$MVPname[z$n==max(z$n)])

}
angels.info3<-function(){
  x=a23fgb
  y=a23.fg
  #y%>%filter(grepl(q,Angels))->y
  #x%>%filter(grepl(q,Angels))->x
  x%>%#group_by(HitterName)%>%
    summarize(G=n(),PA=sum(PlateAppearances,na.rm=TRUE), AB=sum(HitCnt,na.rm=TRUE), 
              H=sum(HittingCnt,na.rm=TRUE), IBB=sum(IntentionalBasesONBallsCnt,na.rm=TRUE), 
              BB=sum(BasesONBallsCnt,na.rm=TRUE), HBP=sum(HitBYPitchCnt,na.rm=TRUE),
              SF=sum(SacrificeFlyCnt,na.rm=TRUE), BA=H/AB, OBP=(H+BB+HBP)/(AB+BB+HBP+SF),
              SLG=sum(TotalBases,na.rm=TRUE)/AB, OPS=OBP+SLG, HR=sum(HomeRunCnt),
              MVP=sum(IsMvp,na.rm=TRUE), SB=sum(StealBaseOKCnt,na.rm=TRUE))->x
  x=x[c("G","PA","BB","H","HR","BA","SB","OBP","SLG","OPS","MVP")]
  x%>%arrange(desc(OPS))->x
  x$BA%>%round(3)%>%format(nsmall=3)->x$BA
  x$OBP%>%round(3)%>%format(nsmall=3)->x$OBP
  x$SLG%>%round(3)%>%format(nsmall=3)->x$SLG
  x$OPS%>%round(3)%>%format(nsmall=3)->x$OPS
  y%>%group_by(MVPname)%>%summarise(n=n())%>%arrange(desc(n))->z
  
  cat("2023樂天桃猿主場紀錄","\n","\n")
  cat("Rakuten Girls:","All","\n")
  cat("出席場數:    ",nrow(y),"場","\n")
  cat("團隊打擊三圍:",x$BA,"/",x$OBP,"/",x$SLG,"\n")
  cat("團隊全壘打數:",x$HR,"\n")
  cat("團隊盜壘數:  ",x$SB,"\n")
  cat("團隊OPS:     ",x$OPS,"\n")
  #cat("獲得最多次MVP的選手:",max(z$n),"次,",z$MVPname[z$n==max(z$n)])
}

ui<-fluidPage(
  titlePanel(h1("從RKG出賽來看打者表現",h4("由PTT wh0386製作"))),
  fluidRow(
    column(4,
           selectInput("Angels","樂天女孩:",c("All",fangels)
           ))
  ),
  #fluidRow(
  #  column(4,
  #         selectInput("dataset","數據",c("打者表現","比賽紀錄")
  #         ))
  #),
  radioButtons("dataset","數據",c("打者表現","比賽紀錄")),
  
  verbatimTextOutput("summary"),
  DT::dataTableOutput("table")
)

server <- function(input, output) {
  
  output$summary<-renderPrint({
    if (input$dataset=="打者表現"){
      if(input$Angels=="All"){
        angels.info3()
      }
      else{angels.info4(input$Angels)}
    }
    
    else{
      if(input$Angels=="All"){
        angels.info1()
      }
      else{angels.info2(input$Angels)}
    }
    
  #if (input$dataset=="比賽紀錄"){  
  #  if (input$Angels=="All"){angels.info1()}
  #  else{angels.info2(input$Angels)}
  #}
    
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    angels.split1<-function(){
      x=a23fgb
      #x%>%filter(grepl(y,Angels))->x
      x%>%group_by(HitterName)%>%
        summarize(G=n(),PA=sum(PlateAppearances,na.rm=TRUE), AB=sum(HitCnt,na.rm=TRUE), 
                  H=sum(HittingCnt,na.rm=TRUE), IBB=sum(IntentionalBasesONBallsCnt,na.rm=TRUE), 
                  BB=sum(BasesONBallsCnt,na.rm=TRUE), HBP=sum(HitBYPitchCnt,na.rm=TRUE),
                  SF=sum(SacrificeFlyCnt,na.rm=TRUE), BA=H/AB, OBP=(H+BB+HBP)/(AB+BB+HBP+SF),
                  SLG=sum(TotalBases,na.rm=TRUE)/AB, OPS=OBP+SLG, HR=sum(HomeRunCnt),
                  SB=sum(StealBaseOKCnt))->x
      x=x[c("HitterName","G","PA","AB","BB","H","HR","SB","BA","OBP","SLG","OPS")]
      colnames(x)[1]="Name"
      x%>%arrange(desc(OPS))->x
      x$BA%>%round(3)%>%format(nsmall=3)->x$BA
      x$OBP%>%round(3)%>%format(nsmall=3)->x$OBP
      x$SLG%>%round(3)%>%format(nsmall=3)->x$SLG
      x$OPS%>%round(3)%>%format(nsmall=3)->x$OPS
      x
    }
    angels.split2<-function(y){
      x=a23fgb
      x%>%filter(grepl(y,Angels))->x
      x%>%group_by(HitterName)%>%
        summarize(G=n(),PA=sum(PlateAppearances,na.rm=TRUE), AB=sum(HitCnt,na.rm=TRUE), 
                  H=sum(HittingCnt,na.rm=TRUE), IBB=sum(IntentionalBasesONBallsCnt,na.rm=TRUE), 
                  BB=sum(BasesONBallsCnt,na.rm=TRUE), HBP=sum(HitBYPitchCnt,na.rm=TRUE),
                  SF=sum(SacrificeFlyCnt,na.rm=TRUE), BA=H/AB, OBP=(H+BB+HBP)/(AB+BB+HBP+SF),
                  SLG=sum(TotalBases,na.rm=TRUE)/AB, OPS=OBP+SLG, HR=sum(HomeRunCnt),
                  SB=sum(StealBaseOKCnt))->x
      x=x[c("HitterName","G","PA","AB","BB","H","HR","SB","BA","OBP","SLG","OPS")]
      colnames(x)[1]="Name"
      x%>%arrange(desc(OPS))->x
      x$BA%>%round(3)%>%format(nsmall=3)->x$BA
      x$OBP%>%round(3)%>%format(nsmall=3)->x$OBP
      x$SLG%>%round(3)%>%format(nsmall=3)->x$SLG
      x$OPS%>%round(3)%>%format(nsmall=3)->x$OPS
      x
    }
    angels.info1<-function(){
      a23.fg->x
      x$Score=paste(x$VisitingScore,":",x$HomeScore,sep=" ")
      x=x[c("Date","Weekdays","VisitingTeamName","Score","AudienceCnt","EndTime","Temperature","MVPname")]
      colnames(x)[1]="日期"
      colnames(x)[2]="星期"
      colnames(x)[3]="客隊"
      colnames(x)[4]="比數"
      colnames(x)[5]="觀眾人數"
      colnames(x)[6]="結束時間"
      colnames(x)[7]="比賽氣溫"
      colnames(x)[8]="單場MVP"
      x
    }
    angels.info2<-function(q){
      a23.fg%>%filter(grepl(q,Angels))->x
      x$Score=paste(x$VisitingScore,":",x$HomeScore,sep=" ")
      x=x[c("Date","Weekdays","VisitingTeamName","Score","AudienceCnt","EndTime","Temperature","MVPname")]
      colnames(x)[1]="日期"
      colnames(x)[2]="星期"
      colnames(x)[3]="客隊"
      colnames(x)[4]="比數"
      colnames(x)[5]="觀眾人數"
      colnames(x)[6]="結束時間"
      colnames(x)[7]="溫度"
      colnames(x)[8]="單場MVP"
      x
    }
    
    
    if (input$dataset=="打者表現"){
      if (input$Angels =="All"){
        angels.split1()
        #angels.info1()
      }
      else{angels.split2(input$Angels)}
    }
    
    else{
      if(input$Angels=="All"){
        angels.info1()
      }
      else{angels.info2(input$Angels)}
    }
    
    
  }))
  
  
  
}

shinyApp(ui = ui, server = server)