library(stringi)
unidf<-read.csv("./unidf.txt",header = TRUE,sep = "")
bidf<-read.csv("./bidf.txt",header = TRUE,sep = "")
tridf<-read.csv("./tridf.txt",header = TRUE,sep = "")










function(input, output) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  ntext <- eventReactive(input$goButton, {
    
    wds<-input$n
    wds <-stri_extract_all_words(wds)
    
    
    ###### unigram code  
    
    if (length(wds[[1]])==1){
      ungrm<-wds
      
      if((ungrm %in% unidf$word)==TRUE){
        
        ungrmprb<-unidf[unidf$word==ungrm,c("prob")]
        
        biprbdf<-bidf[grepl(paste0("^", ungrm, "\\s"), bidf$bigram),c("bigram","biprob")]
        
        
        biprbdf$scndwrdprb<-biprbdf$biprob/ungrmprb
        biprbdf<-biprbdf[with(biprbdf, order(-scndwrdprb)), ]
        
      }
      else{
        
        return ("Unable to predict next word, as the word is not in our dictionary")
      }
      
      
      
      
      
      
      rslt<-sub(ungrm,"",biprbdf$bigram[1:5])
      rslt<-rslt[!is.na(rslt)]
      
      return(rslt) 
      
      
    }
    
    
    #######  bigram code
    
    
    
    
    
    
    else{
      
      bgrm<-tail(wds[[1]],2)
      bgrm<-paste(bgrm,collapse = " ")
      
      if(bgrm %in% bidf$bigram){
        
        
        bgrmprbdf<-bidf[bidf$bigram==bgrm,c("bigram","biprob")]
        triprbdf<-tridf[grepl(paste0("^", bgrm, "\\s"), tridf$trigram),c("trigram","triprob")]
        triprbdf$thrdwrdprb<-triprbdf$triprob/bgrmprbdf$biprob
        rslt<-sub(bgrm,"",triprbdf$trigram[1:5])
        rslt<-rslt[!is.na(rslt)]       
        return (rslt)
        
      }
      
      
      ### backoff to unigram
      else{
        ungrm<-stri_extract_last_words(bgrm)
        
        
        
        
        
        
        
        if(ungrm %in% unidf$word){
          
          ungrmprb<-unidf[unidf$word==ungrm,c("prob")]
          
          biprbdf<-bidf[grepl(paste0("^", ungrm, "\\s"), bidf$bigram),c("bigram","biprob")]
          
          
          biprbdf$scndwrdprb<-biprbdf$biprob/ungrmprb
          biprbdf<-biprbdf[with(biprbdf, order(-scndwrdprb)), ]
          
        }
        else {
          
          return ("Unable to predict next word, as the last word is not in our dictionary")
        }
        
        
        
        
        
        rslt<-sub(ungrm,"",biprbdf$bigram[1:5])
        rslt<-rslt[!is.na(rslt)]
        
        return(rslt) 
        
        
        
      }
      
      
      
    }
    
  })
  
  output$nText <- renderText({
    ntext()
  })
}