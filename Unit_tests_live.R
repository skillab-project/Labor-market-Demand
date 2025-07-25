

source("plumber_analytics_Skillab_fin.R")



library(testthat)

##Form test data
{
  #if(is.null(data))data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  data=list(items=(list(
    list(id="1",#Data scientist
         country="Greece",
         upload_date="2025-02-03",
         occupations=list("http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
                          "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
                          "http://data.europa.eu/esco/isco/C1212"#Human resource managers
         ),
         skills=list("http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
                     "http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d",#Python
                     "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389",#Java
                     "http://data.europa.eu/esco/isced-f/0542",#statistics
                     "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",#English
                     "http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
                     "http://data.europa.eu/esco/skill/484d048c-a5d1-46a5-b57f-45b69c0ac552",#analyse pipeline database information
                     "http://data.europa.eu/esco/skill/001115fb-569f-4ee6-8381-c6807ef2527f",#show initiative
                     "http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb"#lead a team
         ),
         score=4
    ),
    list(id="2",#Data scientist
         country="Germany",
         upload_date="2024-05-19",
         occupations=list("http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
                          "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
                          "http://data.europa.eu/esco/isco/C1212"#Human resource managers
         ),
         skills=list("http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
                     "http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d",#Python
                     "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389",#Java
                     "http://data.europa.eu/esco/isced-f/0542",#statistics
                     "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",#English
                     "http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
                     "http://data.europa.eu/esco/skill/484d048c-a5d1-46a5-b57f-45b69c0ac552",#analyse pipeline database information
                     "http://data.europa.eu/esco/skill/001115fb-569f-4ee6-8381-c6807ef2527f",#show initiative
                     "http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb"#lead a team
         ),
         score=5
    ),
    list(id="0",#Doctors and data scientist
         country="Greece",
         upload_date="2025-05-13",
         occupations=list("http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
                          "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
                          "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7"#speech and language therapist
         ),
         skills=list(
           "http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
           "http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
           "http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb",#lead a team
           "http://data.europa.eu/esco/isced-f/0912" #medicine
         ),
         score=2
    ),
    list(id="3",#Doctors and data scientist
         country="Greece",
         upload_date="2024-05-13",
         occupations=list("http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
                          "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
                          "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7"#speech and language therapist
         ),
         skills=list(
           "http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
           "http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
           "http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb",#lead a team
           "http://data.europa.eu/esco/isced-f/0912" #medicine
         ),
         score=4
    ),
    list(id="01",#HR with R
         country="Germany",
         upload_date="2022-01-22",
         occupations=list("http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
                          "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75" #talent acquisition manager
         ),
         skills=list(
           "http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
           "http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb",#lead a team
           "http://data.europa.eu/esco/skill/cb668e89-6ef5-4ff3-ab4a-506010e7e70b"#manage a team
         ),
         score=1
    ) 
    
  )))
  
  
  data$count=length(data$items)
  
  
}



data_now_temp_skills <- do.call(
  rbind,
  lapply(data$items, function(x) {
    if (length(x[["skills"]]) > 0 & !is.null(x[["skills"]])) {
      return(data.frame(item_id = x$id, item = unique(unlist(x[["skills"]])), stringsAsFactors = FALSE))
    }
  })
)

data_now_temp_occupations <- do.call(
  rbind,
  lapply(data$items, function(x) {
    if (length(x[["occupations"]]) > 0 & !is.null(x[["occupations"]])) {
      return(data.frame(item_id = x$id, item = unique(unlist(x[["occupations"]])), stringsAsFactors = FALSE))
    }
  })
)

data_now_temp_country <- do.call(
  rbind,
  lapply(data$items, function(x) {
    if (length(x[["country"]]) > 0 & !is.null(x[["country"]])) {
      return(data.frame(item_id = x$id, item = unique(unlist(x[["country"]])), stringsAsFactors = FALSE))
    }
  })
)


data_now_temp_dates <- do.call(
  rbind,
  lapply(data$items, function(x) {
    if (length(x[["upload_date"]]) > 0 & !is.null(x[["upload_date"]])) {
      return(data.frame(item_id = x$id, item = unique(unlist(x[["upload_date"]])), stringsAsFactors = FALSE))
    }
  })
)



test_that("double_to_occur works",{
  
  mat_country <- cbind(Germany = c(0,1,0,1,0), Greece = c(1,0,1,0,1))
  rownames(mat_country) <- c("0","01","1","2","3")  
  
  expect_equal(double_to_occur(data = data_now_temp_country),expected = mat_country)
  
  mat_occs <- cbind(c(0,0,1,1,0),c(0,0,1,1,0),c(1,1,1,1,1),c(1,0,0,0,1),c(1,0,0,0,1),c(0,1,0,0,0))
  colnames(mat_occs)=c(
    "http://data.europa.eu/esco/isco/C1212",
    "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
    "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
    "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
    "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
    "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"
  )
  rownames(mat_occs) <- c("0","01","1","2","3")  
  
  expect_equal(double_to_occur(data = data_now_temp_occupations),expected = mat_occs )
  
})


test_that("propagation_skills works",{
  
  urls_now=c(
    "http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
    "http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d" #Python
  )

#1
{  
  mat_exp=data.frame(
    Original_id=c(urls_now[1],urls_now[1],urls_now[2]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/isced-f/05",
      "http://data.europa.eu/esco/isced-f/06",
      "http://data.europa.eu/esco/isced-f/06"
    )
  )
  
  mat_fun=propagation_skills(urls = urls_now,pillar = "Knowledge",level = "1")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
}
  
#2
{  
  mat_exp=data.frame(
    Original_id=c(urls_now[1],urls_now[1],urls_now[2]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/isced-f/054",
      "http://data.europa.eu/esco/isced-f/061",
      "http://data.europa.eu/esco/isced-f/061"
    )
  )
    
  mat_fun=propagation_skills(urls = urls_now,pillar = "Knowledge",level = "2")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
}
  
#3
{
  mat_exp=data.frame(
    Original_id=c(urls_now[1],urls_now[1],urls_now[2]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/isced-f/0542",
      "http://data.europa.eu/esco/isced-f/0613",
      "http://data.europa.eu/esco/isced-f/0613"
    )
  )
  
  mat_fun=propagation_skills(urls = urls_now,pillar = "Knowledge",level = "3")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
}
  
#4 
{
  mat_exp=data.frame(
    Original_id=c(urls_now[1],urls_now[1],urls_now[2],urls_now[2]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/isced-f/054",
      "http://data.europa.eu/esco/isced-f/061",
      "http://data.europa.eu/esco/isced-f/061",
      "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9"
    )
  )
  
  mat_fun=propagation_skills(urls = urls_now,pillar = "Knowledge;;Skill",level = "2;;2")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
  
}

urls_now=c("http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb")#lead a team
  
#5
{
  mat_exp=data.frame(
    Original_id=c(urls_now[1]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb"
    )
  )
  
  mat_fun=propagation_skills(urls = urls_now,pillar = "Traversal",level = "2")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
  
}

urls_now=c("http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0")#English

#6
{
  mat_exp=data.frame(
    Original_id=c(urls_now[1]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0"
    )
  )
  
  mat_fun=propagation_skills(urls = urls_now,pillar = "Language",level = "2")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
  
}

#7
{
  mat_exp=data.frame(
    Original_id=c(urls_now[1],urls_now[1]),
    Ancestors_now=c(
      "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",
      "http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10"
    )
  )
  
  mat_fun=propagation_skills(urls = urls_now,pillar = "Language;;Traversal",level = "2;;2")
  mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
  
  rownames(mat_exp)=rownames(mat_fun)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
}
  
})


test_that("propagation_occupation works",{
  
  urls_now=c(
    "http://data.europa.eu/esco/isco/C1212", #HR managers
    "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341", #computer vision engineer
    "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30", #data scientist
    "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db", #bioinformatics scientist
    "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7", #speech and language therapist
    "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"  #talent acquisition manager
    
  )

  
  #1
  {
    mat_exp=data.frame(
      Original_id=c(urls_now),
      Ancestors_now=c(
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/isco/C2511",
        "http://data.europa.eu/esco/isco/C2511",
        "http://data.europa.eu/esco/isco/C2131",
        "http://data.europa.eu/esco/isco/C2266",
        "http://data.europa.eu/esco/isco/C1212"
        
      )
    )
    
    
    #"http://data.europa.eu/esco/isco/C1212",#Human resource managers
    #"http://data.europa.eu/esco/isco/C2131",#Biologists, botanists, zoologists and related professionals
    #"http://data.europa.eu/esco/isco/C2266",#Audiologists and speech therapists (speech and language therapist)
    #"http://data.europa.eu/esco/isco/C2511" #Systems Analysts
    
    mat_fun=propagation_occupations(urls = urls_now,level = "3")
    mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
    
    rownames(mat_exp)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_exp)
      
  }
  
  #2
  {
    mat_exp=data.frame(
      Original_id=c(urls_now),
      Ancestors_now=c(
        "http://data.europa.eu/esco/isco/C121",
        "http://data.europa.eu/esco/isco/C251",
        "http://data.europa.eu/esco/isco/C251",
        "http://data.europa.eu/esco/isco/C213",
        "http://data.europa.eu/esco/isco/C226",
        "http://data.europa.eu/esco/isco/C121"
        
      )
    )
    
    mat_fun=propagation_occupations(urls = urls_now,level = "2")
    mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
    
    rownames(mat_exp)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_exp)
    
  }

  
  #3
  {
    mat_exp=data.frame(
      Original_id=c(urls_now),
      Ancestors_now=c(
        "http://data.europa.eu/esco/isco/C12",
        "http://data.europa.eu/esco/isco/C25",
        "http://data.europa.eu/esco/isco/C25",
        "http://data.europa.eu/esco/isco/C21",
        "http://data.europa.eu/esco/isco/C22",
        "http://data.europa.eu/esco/isco/C12"
        
      )
    )
    
    mat_fun=propagation_occupations(urls = urls_now,level = "1")
    mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
    
    rownames(mat_exp)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_exp)
    
  }
  
  
  #4
  {
    mat_exp=data.frame(
      Original_id=c(urls_now),
      Ancestors_now=c(
        "http://data.europa.eu/esco/isco/C1",
        "http://data.europa.eu/esco/isco/C2",
        "http://data.europa.eu/esco/isco/C2",
        "http://data.europa.eu/esco/isco/C2",
        "http://data.europa.eu/esco/isco/C2",
        "http://data.europa.eu/esco/isco/C1"
        
      )
    )
    
    mat_fun=propagation_occupations(urls = urls_now,level = "0")
    mat_fun=mat_fun[order(mat_fun$Original_id,mat_fun$Ancestors_now),]
    
    rownames(mat_exp)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_exp)
    
  }
  
})


test_that("double_to_occur_with_propagate works",{
  
  #1 Occupations
  {
    mat_occs_prop <- cbind(c(0,1,1,1,0),c(1,0,0,0,1),c(1,0,0,0,1),c(1,1,1,1,1))
    colnames(mat_occs_prop)=c(
      "http://data.europa.eu/esco/isco/C1212",#Human resource managers
      "http://data.europa.eu/esco/isco/C2131",#Biologists, botanists, zoologists and related professionals
      "http://data.europa.eu/esco/isco/C2266",#Audiologists and speech therapists (speech and language therapist)
      "http://data.europa.eu/esco/isco/C2511" #Systems Analysts
    )
    rownames(mat_occs_prop) <- c("0","01","1","2","3")  
    
    expect_equal(return_double_to_occur_propagate(data = data,what="occupations",level = "3"),expected = mat_occs_prop)
    
  }
  
  #2 skills
  {
    
    mat_skills_prop <- cbind(c(1,1,1,1,1),c(1,1,1,1,1),c(1,0,0,0,1),c(1,0,1,1,1),c(0,0,1,1,0),c(0,0,1,1,0),c(0,0,1,1,0),c(1,1,1,1,1),c(0,0,1,1,0),c(0,0,1,1,0),c(0,0,1,1,0),c(1,1,1,1,1))
    colnames(mat_skills_prop)=c(
      "http://data.europa.eu/esco/isced-f/054",#mathematics and statistics
      "http://data.europa.eu/esco/isced-f/061",#ICTs
      "http://data.europa.eu/esco/isced-f/091",#health
      "http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943", #accessing and analysing digital data
      "http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80", #analysing and evaluating information and data
      "http://data.europa.eu/esco/skill/66fdc34c-2326-4baa-b8ff-7a1d1015fe3a", #planning and organising
      "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0", #English
      "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a", #supervising people
      "http://data.europa.eu/esco/skill/91860993-1a8b-4473-91f3-600aa1924bd0", #taking a proactive approach
      "http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10", #mastering languages
      "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9", #using digital tools for collaboration, content creation and problem solving
      "http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb" #leading others
      
    )
    rownames(mat_skills_prop) <- c("0","01","1","2","3")  
    
    expect_equal(
      return_double_to_occur_propagate(data = data,what="skills",pillar="Skill;;Knowledge;;Language;;Traversal",level="2;;2;;2;;2"),
      expected = mat_skills_prop)
    
  }
  
})


test_that("double_to_double_with_propagate works",{
  
  mat_exp=data.frame(
    item_id=c(
      "0","01","1","2","3",
      "0","01","1","2","3",
      "0","3",
      "0","1","2","3",
      "1","2",
      "1","2",
      "1","2",
      "0","01","1","2","3",
      "1","2",
      "1","2",
      "1","2",
      "0","01","1","2","3"
    ),
    Ancestors_now=c(rep("http://data.europa.eu/esco/isced-f/054",5),
                    rep("http://data.europa.eu/esco/isced-f/061",5),
                    rep("http://data.europa.eu/esco/isced-f/091",2),
                    rep("http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943",4),
                    rep("http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80",2),
                    rep("http://data.europa.eu/esco/skill/66fdc34c-2326-4baa-b8ff-7a1d1015fe3a",2),
                    rep("http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",2),
                    rep("http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",5),
                    rep("http://data.europa.eu/esco/skill/91860993-1a8b-4473-91f3-600aa1924bd0",2),
                    rep("http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10",2),
                    rep("http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9",2),
                    rep("http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb",5)
    )
  )
  
  mat_fun=return_double_to_occur_propagate(data = data,what = "skills",pillar="Skill;;Knowledge;;Language;;Traversal",level="2;;2;;2;;2",is_mat = F)
  mat_fun=mat_fun[order(mat_fun$Ancestors_now,mat_fun$item_id),]
  
  rownames(mat_fun)=rownames(mat_exp)=NULL
  
  expect_equal(mat_fun,expected = mat_exp)
  
})


test_that("co_occurence_weight works",{
  mat_base=matrix(c(5,3,1,3,10,2,1,2,7),
    ncol=3,nrow=3
  )
  
  no_documents=13
  
  #ii_weight
  {
    mat_exp=matrix(c(5,0.6,0.2,0.6,10,0.2857143,0.2,0.2857143,7),
                   ncol=3,nrow=3)
    mat_fun=co_occurence_weight(data = mat_base,weight = "ii_weight",no_documents = no_documents)
    
    expect_equal(mat_fun,expected = mat_exp,tolerance = 0.00001) 
  }
  
  #ji_weight
  {
    mat_exp=matrix(c(5,0.25,0.09090909,0.25,10,0.1333333,0.09090909,0.1333333,7),
                              ncol=3,nrow=3)
    mat_fun=co_occurence_weight(data = mat_base,weight = "ji_weight",no_documents = no_documents)
    
    expect_equal(mat_fun,expected = mat_exp,tolerance = 0.00001)
  }
  
  #ei_weight
  {
    mat_exp=matrix(c(5,0.18,0.02857143,0.18,10,0.05714286,0.02857143,0.05714286,7),
                   ncol=3,nrow=3)
    mat_fun=co_occurence_weight(data = mat_base,weight = "ei_weight",no_documents = no_documents)
    
    expect_equal(mat_fun,expected = mat_exp,tolerance = 0.00001) 
  }
  


  
})


test_that("query_pos_fun works",{
  
  #Occupations
  {
    query_feat="occupations";query_value="http://data.europa.eu/esco/isco/C1212"
    pos_true=c(1,2)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    
    expect_equal(pos_fun,expected = pos_true)
  }
  
  
  #Country
  {
    query_feat="country";query_value="Greece"
    pos_true=c(1,3,4)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
    
  }
  
  #Occupations propagation
  {
    query_feat="occupation_propagation";query_value="http://data.europa.eu/esco/isco/C1212"
    pos_true=c(1,2,5)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  #Skills propagation
  {
    query_feat="skill_propagation";query_value="http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943"
    
    pos_true=c(1,2,3,4)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }

  
  
  #NUmeric data  -> EQ
  {
    query_feat="!!EQ_score";query_value="4"
    
    pos_true=c(1,4)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  #NUmeric data  -> GE
  {
    query_feat="!!GE_score";query_value="4"
    
    pos_true=c(1,2,4)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  
  #NUmeric data  -> GR
  {
    query_feat="!!GR_score";query_value="4"
    
    pos_true=c(2)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  #Numeric data -> LE
  {
    query_feat="!!LE_score";query_value="4"
    
    pos_true=c(1,3,4,5)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  #Numeric data -> LO
  {
    query_feat="!!LO_score";query_value="4"
    
    pos_true=c(3,5)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  
  
  ###Date data -> LE 2024
  {
    query_feat="!?LEY_upload_date";query_value="2024"
    
    pos_true=c(2,4,5)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  ###Date data -> LO month 04
  {
    query_feat="!?LEM_upload_date";query_value="04"
    
    pos_true=c(1,5)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
  ###Date data -> GR year_mont 2024-05
  {
    query_feat="!?GRB_upload_date";query_value="2024-05"
    
    pos_true=c(1,3)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }

  ###Date data -> EQ year 2024
  {
    query_feat="!?EQY_upload_date";query_value="2024"
    
    pos_true=c(2,4)
    
    pos_fun=query_pos_fun(data,target_feature = query_feat,target_value = query_value)
    expect_equal(pos_fun,expected = pos_true)
  }
  
})


test_that("multi_query_fun works",{
  
  
  
  query_feat="occupations;;!!GE_score";query_value="http://data.europa.eu/esco/isco/C1212;;4"
  
  #OR
  {
    pos_true=c(1,2,4)
    pos_fun=multi_query_fun(data = data,target_query  = query_feat,values_query  = query_value,type_query = "OR")
    expect_equal(pos_fun,expected = pos_true)
  }
  
  #AND
  {
    pos_true=c(1,2)
    pos_fun=multi_query_fun(data = data,target_query  = query_feat,values_query  = query_value,type_query = "AND")
    expect_equal(pos_fun,expected = pos_true)
  }
  
  
})


test_that("analytics_fun works",{
  
  #Countries
  {
    
    mat_real=data.frame(Var1=factor(c("Greece","Germany"),levels = c("Greece","Germany")),Freq=c(3,2))
    mat_real=list(
      country=mat_real
    )
    
    mat_fun=analytics_fun(data = data,features_query = "country")
    
    expect_equal(mat_fun,expected = mat_real)
    
  }

  
  #Skills
  {
    skill_urls=c("http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb",#lead a team
                 "http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
                 "http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
                 "http://data.europa.eu/esco/isced-f/0542",#statistics
                 "http://data.europa.eu/esco/isced-f/0912",#medicine
                 "http://data.europa.eu/esco/skill/001115fb-569f-4ee6-8381-c6807ef2527f",#show initiative
                 "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389",#Java (computer programming)
                 "http://data.europa.eu/esco/skill/484d048c-a5d1-46a5-b57f-45b69c0ac552",#analyse pipeline database information
                 "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",#English
                 "http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d",#Python (computer programming)
                 "http://data.europa.eu/esco/skill/cb668e89-6ef5-4ff3-ab4a-506010e7e70b"#manage a team
                 
                 )
    mat_real=data.frame(Var1=factor(skill_urls,
                                    levels = skill_urls),
                        Freq=c(5,5,4,2,2,2,2,2,2,2,1),
                        label=c(
                          "lead a team",
                          "R",
                          "perform data analysis",
                          "statistics",
                          "medicine",
                          "show initiative",
                          "Java (computer programming)",
                          "analyse pipeline database information",
                          "English",
                          "Python (computer programming)",
                          "manage a team"
                        )
                        )
    mat_real=list(
      skills=mat_real
    )
    
    mat_fun=analytics_fun(data = data,features_query = "skills")
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
  #Occupations
  {
    occupation_urls=c(
      
      "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
      "http://data.europa.eu/esco/isco/C1212",#Human resource managers
      "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
      "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
      "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",#speech and language therapist
      "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75" #talent acquisition manager
      
    )
    
    mat_real=data.frame(
      Var1=c(
        factor(occupation_urls,levels = occupation_urls)
      ),
      Freq=c(5,2,2,2,2,1),
      label=c(
        "data scientist",
        "Human resource managers",
        "computer vision engineer",
        "bioinformatics scientist",
        "speech and language therapist",
        "talent acquisition manager"
      )
    )
    
    mat_real=list(
      occupations=mat_real
    )
    
    mat_fun=analytics_fun(data = data,features_query = "occupations")
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
})


test_that("analytics_prop_fun works",{
  
  #1 skills
  {
    url_skills=c(
      "http://data.europa.eu/esco/isced-f/054",
      "http://data.europa.eu/esco/isced-f/061",
      "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",
      "http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb",
      "http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943",
      "http://data.europa.eu/esco/isced-f/091",
      "http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80",
      "http://data.europa.eu/esco/skill/66fdc34c-2326-4baa-b8ff-7a1d1015fe3a",
      "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",
      "http://data.europa.eu/esco/skill/91860993-1a8b-4473-91f3-600aa1924bd0",
      "http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10",
      "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9"
    )
    
    mat_real=data.frame(
      Id=url_skills,
      Frequency=c(5,5,5,5,4,2,2,2,2,2,2,2),
      Label=c(
              "mathematics and statistics",
              "information and communication technologies (icts)",
              "supervising people",
              "leading others",
              "accessing and analysing digital data",
              "health",
              "analysing and evaluating information and data",
              "planning and organising",
              "English",
              "taking a proactive approach",
              "mastering languages",
              "using digital tools for collaboration, content creation and problem solving"
              )
    )
    
    mat_fun=analytics_prop_fun(data=data,what="skills",pillar="Skill;;Knowledge;;Language;;Traversal",level="2;;2;;2;;2")#ok
    
    rownames(mat_fun)=  rownames(mat_real)= NULL

    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  #2 occupations
  {
    url_occupations=c(
      "http://data.europa.eu/esco/isco/C2511",#Systems analysts
      "http://data.europa.eu/esco/isco/C1212",#Human resource managers
      "http://data.europa.eu/esco/isco/C2131",#Biologists, botanists, zoologists and related professionals (bioinformatics scientist)
      "http://data.europa.eu/esco/isco/C2266"#Audiologists and speech therapists (speech and language therapist)
    )
    mat_real=data.frame(
      Id=url_occupations,
      Frequency=c(5,3,2,2),
      Label=c(
        "Systems analysts",
        "Human resource managers",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists"
      )
    )
    
    mat_fun=analytics_prop_fun(data=data,what="occupations",level="3")#ok
    
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
 
})


test_that("exploratory_fun works",{
  
  #1 country - occupations
  {
    mat_real=data.frame(
      Var1=factor(c(
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"
        ),levels = c(
          "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
          "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
          "http://data.europa.eu/esco/isco/C1212",
          "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
          "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
          "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"
        )),
      Var2=factor(c(
        "Greece",
        "Greece",
        "Greece",
        "Germany",
        "Greece",
        "Greece",
        "Germany",
        "Germany",
        "Germany"
      ),levels = c("Greece","Germany")),
      Freq=c(3,2,2,2,1,1,1,1,1),
      item_1=c(
        "data scientist",
        "bioinformatics scientist",
        "speech and language therapist",
        "data scientist",
        "computer vision engineer",
        "Human resource managers",
        "computer vision engineer",
        "Human resource managers",
        "talent acquisition manager"
      )
      
    )
    
    
    mat_fun=exploratory_fun(data = data,features_query = "occupations;;country")
    
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
  #2 occupation - occupation
  {
    mat_real=data.frame(
      c1=c(
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30"
      ),
      c2=c(
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"
        
      ),
      Freq=c(2,2,2,2,2,2,1),
      item_1=c(
        "Human resource managers",
        "Human resource managers",
        "computer vision engineer",
        "data scientist",
        "data scientist",
        "bioinformatics scientist",
        "data scientist"
      ),
      item_2=c(
        "computer vision engineer",
        "data scientist",
        "data scientist",
        "bioinformatics scientist",
        "speech and language therapist",
        "speech and language therapist",
        "talent acquisition manager"
      )
    )
    
    
    mat_fun=exploratory_fun(data = data,features_query = "occupations;;occupations")
    
    mat_real$c1=factor(mat_real$c1,levels=levels(mat_fun$c1))
    mat_real$c2=factor(mat_real$c2,levels=levels(mat_fun$c2))
    
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
})


test_that("exploratory_fun_prop works",{
  
  
  
  #1 skills_prop (skill , traversal) - occupations_prop
  {
    
    #a=readxl::read_xlsx("occ_prop_skill_prop_skillpillar.xlsx")
    mat_real=data.frame(
      Var1=c('http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C2131',
             'http://data.europa.eu/esco/isco/C2266',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C2131',
             'http://data.europa.eu/esco/isco/C2266',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C1212',
             'http://data.europa.eu/esco/isco/C2511',
             'http://data.europa.eu/esco/isco/C2131',
             'http://data.europa.eu/esco/isco/C2266'
      ),
      Var2=c('http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a',
             'http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb',
             'http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943',
             'http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a',
             'http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb',
             'http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943',
             'http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943',
             'http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943',
             'http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80',
             'http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80',
             'http://data.europa.eu/esco/skill/66fdc34c-2326-4baa-b8ff-7a1d1015fe3a',
             'http://data.europa.eu/esco/skill/66fdc34c-2326-4baa-b8ff-7a1d1015fe3a',
             'http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a',
             'http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a',
             'http://data.europa.eu/esco/skill/91860993-1a8b-4473-91f3-600aa1924bd0',
             'http://data.europa.eu/esco/skill/91860993-1a8b-4473-91f3-600aa1924bd0',
             'http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10',
             'http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10',
             'http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9',
             'http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9',
             'http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb',
             'http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb'),
      Freq=c(5,5,4,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),
      item_1=c(
        "Systems analysts",
        "Systems analysts",
        "Systems analysts",
        "Human resource managers",
        "Human resource managers",
        "Human resource managers",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists",
        "Human resource managers",
        "Systems analysts",
        "Human resource managers",
        "Systems analysts",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists",
        "Human resource managers",
        "Systems analysts",        
        "Human resource managers",
        "Systems analysts",
        "Human resource managers",
        "Systems analysts",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists"
      ),
      item_2=c(
        "supervising people",
        "leading others",
        "accessing and analysing digital data",
        "supervising people",
        "leading others",
        "accessing and analysing digital data",
        "accessing and analysing digital data",
        "accessing and analysing digital data",
        "analysing and evaluating information and data",
        "analysing and evaluating information and data",
        "planning and organising",
        "planning and organising",
        "supervising people",
        "supervising people",
        "taking a proactive approach",
        "taking a proactive approach",
        "mastering languages",
        "mastering languages",
        "using digital tools for collaboration, content creation and problem solving",
        "using digital tools for collaboration, content creation and problem solving",
        "leading others",
        "leading others"
        
      )
    )
    
    
    
    mat_fun=exploratory_fun_prop(data = data,level_1 = "3",level_2 = "2;;2",pillar_2 = "Skill;;Traversal",features_query = "occupations;;skills")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
    
    
    
  }
  
  
  #2 occupations_prop - countries
  {
    url_occupations=c(
      "http://data.europa.eu/esco/isco/C2511",#Systems analysts
      "http://data.europa.eu/esco/isco/C1212",#Human resource managers
      "http://data.europa.eu/esco/isco/C2131",#Biologists, botanists, zoologists and related professionals (bioinformatics scientist)
      "http://data.europa.eu/esco/isco/C2266"#Audiologists and speech therapists (speech and language therapist)
    )
    
    mat_real=data.frame(
      Var1=c(
        "http://data.europa.eu/esco/isco/C2511",#Systems analysts
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/isco/C2511",#Systems analysts
        "http://data.europa.eu/esco/isco/C2131",#Biologists, botanists, zoologists and related professionals (bioinformatics scientist)
        "http://data.europa.eu/esco/isco/C2266",#Audiologists and speech therapists (speech and language therapist)
        "http://data.europa.eu/esco/isco/C1212"#Human resource managers
        
      ),
      Var2=c(
        "Greece",
        "Germany",
        "Germany",
        "Greece",
        "Greece",
        "Greece"
      ),
      Freq=c(3,2,2,2,2,1),
      item_1=c(
        "Systems analysts",
        "Human resource managers",
        "Systems analysts",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists",
        "Human resource managers"
      )
      
    )
    
    
    mat_fun=exploratory_fun_prop(data = data,level_1 = "3",features_query = "occupations;;country")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  

  #3  countries - skills_prop (skill)
  {
    
    mat_real=data.frame(
      Var1=c("Greece",
             "Greece",
             "Germany",
             "Germany",
             "Germany",
             "Greece",
             "Germany",
             "Greece"
             ),
      Var2=c(
        "http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943",
        "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",
        "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",
        "http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943",
        "http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80",
        "http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80",
        "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9",
        "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9"
      ),
      Freq=c(3,3,2,1,1,1,1,1),
      item_2=c(
        "accessing and analysing digital data",
        "supervising people",
        "supervising people",
        "accessing and analysing digital data",
        "analysing and evaluating information and data",
        "analysing and evaluating information and data",
        "using digital tools for collaboration, content creation and problem solving",
        "using digital tools for collaboration, content creation and problem solving"
      )
    )
    
    
    mat_fun=exploratory_fun_prop(data = data,level_2 = "2",pillar_2 = "Skill",features_query = "country;;skills")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
  #4 occupations_prop - occupations_prop
  {
    mat_real=data.frame(
      Var1=c(
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C1212',
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C1212',
        'http://data.europa.eu/esco/isco/C2131',
        'http://data.europa.eu/esco/isco/C2266',
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C2131',
        'http://data.europa.eu/esco/isco/C2266',
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C2131',
        'http://data.europa.eu/esco/isco/C2266'
      ),
      Var2=c(
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C1212',
        'http://data.europa.eu/esco/isco/C1212',
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C2131',
        'http://data.europa.eu/esco/isco/C2131',
        'http://data.europa.eu/esco/isco/C2131',
        'http://data.europa.eu/esco/isco/C2266',
        'http://data.europa.eu/esco/isco/C2266',
        'http://data.europa.eu/esco/isco/C2266',
        'http://data.europa.eu/esco/isco/C2511',
        'http://data.europa.eu/esco/isco/C2511'
      ),
      Freq=c(5,3,3,3,2,2,2,2,2,2,2,2),
      item_1=c(
        "Systems analysts",
        "Human resource managers",
        "Systems analysts",
        "Human resource managers",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists",
        "Systems analysts",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists",
        "Systems analysts",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists"
        
      ),
      item_2=c(
        "Systems analysts",
        "Human resource managers",
        "Human resource managers",
        "Systems analysts",
        "Biologists, botanists, zoologists and related professionals",
        "Biologists, botanists, zoologists and related professionals",
        "Biologists, botanists, zoologists and related professionals",
        "Audiologists and speech therapists",
        "Audiologists and speech therapists",
        "Audiologists and speech therapists",
        "Systems analysts",
        "Systems analysts"
        
      )
    )
    
    
    mat_fun=exploratory_fun_prop(data = data,level_1 = "3",level_2 = "3",features_query = "occupations;;occupations")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    rownames(mat_fun)=  rownames(mat_real)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
})


test_that("exploratory_fun_one",{
  
  #Occupation and Country -> skills Java or manage a team
  {
    mat_real=list(
      occupations=data.frame(Var1=c(
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75" #talent acquisition manager
      ),
      Freq=c(
        3,
        2,
        2,
        1
      )
      ),
      country=data.frame(
        Var1=c(
          "Germany",
          "Greece"
        ),
        Freq=c(
          2,
          1
        )
      ),
      counter=3
    )
    
    
    mat_fun=exploratory_fun_one(data = data,features_query = "occupations;;country",
                                target_feature = "skills",
                                target_values ="http://data.europa.eu/esco/skill/cb668e89-6ef5-4ff3-ab4a-506010e7e70b;;http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389"
                                
    )
    
    mat_real$occupations$Var1=factor(mat_real$occupations$Var1,levels=levels(mat_fun$occupations$Var1))
    mat_real$country$Var1=factor(mat_real$country$Var1,levels=levels(mat_fun$country$Var1))
    
    rownames(mat_fun$occupations)=  rownames(mat_real$occupations)= NULL
    rownames(mat_fun$country)=  rownames(mat_real$country)= NULL
    
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
  ##skills - occupations Human resource manager or bioinformatics scientist
  {
    mat_real=list(
      skills=data.frame(Var1=c(
        "http://data.europa.eu/esco/skill/1f1d2ff8-c4c1-45cc-9812-6a7ee84a73cb",#lead a team
        "http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
        "http://data.europa.eu/esco/skill/51586df8-1c46-4b47-8583-773cb63bf00b",#R
        "http://data.europa.eu/esco/isced-f/0542",#statistics
        "http://data.europa.eu/esco/isced-f/0912", #medicine
        "http://data.europa.eu/esco/skill/001115fb-569f-4ee6-8381-c6807ef2527f",#show initiative
        "http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389",#Java
        "http://data.europa.eu/esco/skill/484d048c-a5d1-46a5-b57f-45b69c0ac552",#analyse pipeline database information
        "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",#English
        "http://data.europa.eu/esco/skill/ccd0a1d9-afda-43d9-b901-96344886e14d"#Python
      ),
      Freq=c(
        4,
        4,
        4,
        2,
        2,
        2,
        2,
        2,
        2,
        2
      )
      ),
      counter=4
    )
    
    
    mat_fun=exploratory_fun_one(data = data,features_query = "skills",
                                target_feature = "occupations",
                                target_values ="http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db;;http://data.europa.eu/esco/isco/C1212"
                                
    )
    
    mat_real$skills$Var1=factor(mat_real$skills$Var1,levels=levels(mat_fun$skills$Var1))
    
    rownames(mat_fun$skills)=  rownames(mat_real$skills)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
})


test_that("exploratory_fun_one_prop",{
  
  
  ####occupation propagation - skills Java , medicine
  {
    mat_real=data.frame(Item=c(
      "http://data.europa.eu/esco/isco/C2511",
      "http://data.europa.eu/esco/isco/C1212",
      "http://data.europa.eu/esco/isco/C2131",
      "http://data.europa.eu/esco/isco/C2266"
    ),
    Freq=c(
      4,
      2,
      2,
      2
    ),
    occupations=c(
      "Systems analysts",
      "Human resource managers",
      "Biologists, botanists, zoologists and related professionals",
      "Audiologists and speech therapists"
    )
    )
    
    
    mat_fun=exploratory_fun_one_prop(data = data,features_query = "occupations",
                                     target_feature = "skills",
                                     target_values ="http://data.europa.eu/esco/skill/19a8293b-8e95-4de3-983f-77484079c389;;http://data.europa.eu/esco/isced-f/0912",
                                     pillar = "",
                                     level = "3"
    )
    
    rownames(mat_fun)=rownames(mat_real)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
  ####skills propagation  - occupation propagation Human resource managers
  {
    mat_real=data.frame(Item=c(
      "http://data.europa.eu/esco/skill/fe5eabaa-63f6-4c44-b405-fc3ded8d56cb",#leading others
      "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",#English
      "http://data.europa.eu/esco/skill/9adca63b-9c75-4804-8dab-d62872540a10",#mastering languages
      "http://data.europa.eu/esco/skill/66fdc34c-2326-4baa-b8ff-7a1d1015fe3a",#planning and organising
      "http://data.europa.eu/esco/skill/91860993-1a8b-4473-91f3-600aa1924bd0" #taking a proactive approach
    ),
    Freq=c(
      3,
      2,
      2,
      2,
      2
    ),
    skills=c(
      "leading others",
      "English",
      "mastering languages",
      "planning and organising",
      "taking a proactive approach"
    )
    )
    
    
    mat_fun=exploratory_fun_one_prop(data = data,features_query = "skills",
                                     target_feature = "occupations_prop",
                                     target_values ="http://data.europa.eu/esco/isco/C1212",
                                     pillar = "Traversal;;Language",
                                     level = "2;;2"
    )
    
    rownames(mat_fun)=rownames(mat_real)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
  ####countries - occupation propagation Human resource managers
  {
    mat_real=data.frame(Item=c(
      "Germany",
      "Greece"
      
    ),
    Freq=c(
      2,
      1
      
    )
    )
    
    
    mat_fun=exploratory_fun_one_prop(data = data,features_query = "country",
                                     target_feature = "occupations_prop",
                                     target_values ="http://data.europa.eu/esco/isco/C1212",
                                     pillar = "",
                                     level = ""
    )
    
    rownames(mat_fun)=rownames(mat_real)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
  ####countries - skill propagation 
  {
    mat_real=data.frame(Item=c(
      "Greece"
    ),
    Freq=c(
      2
    )
    )
    
    
    mat_fun=exploratory_fun_one_prop(data = data,features_query = "country",
                                     target_feature = "skills_prop",
                                     target_values ="http://data.europa.eu/esco/isced-f/091",
                                     pillar = "",
                                     level = ""
    )
    
    rownames(mat_fun)=rownames(mat_real)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
  ####occupations - skill propagation 
  {
    mat_real=data.frame(Item=c(
      "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
      "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
      "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7"#speech and language therapist
    ),
    Freq=c(
      2,
      2,
      2
    ),
    occupations=c(
      "data scientist",
      "bioinformatics scientist",
      "speech and language therapist"
    )
    )
    
    
    
    mat_fun=exploratory_fun_one_prop(data = data,features_query = "occupations",
                                     target_feature = "skills_prop",
                                     target_values ="http://data.europa.eu/esco/isced-f/091",
                                     pillar = "",
                                     level = ""
    )
    
    rownames(mat_fun)=rownames(mat_real)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
})


test_that("trend_anal_fun works",{
  
  
  #Countries - years
  {
    
    mat_real=list(country=data.frame(
      date=c(2025,2022,2024,2024),
      item=c("Greece","Germany","Germany","Greece"),
      Freq=c(2,1,1,1)
    ))
    
    mat_fun=trend_anal_fun(data = data,features_query = "country","what"="year")
    
    mat_real$country$date=factor(mat_real$country$date,levels = levels(mat_fun$country$date))
    mat_real$country$item=factor(mat_real$country$item,levels = levels(mat_fun$country$item))
    rownames(mat_fun$country)=  rownames(mat_real$country)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  #Occupations - years
  {

    
    mat_real=list(
      occupations=data.frame(
      date=c(
        2024,
        2025,
        2024,
        2025,
        2024,
        2025,
        2022,
        2024,
        2025,
        2024,
        2025,
        2022
      ),
      item=c(
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"
      ),
      Freq=c(
        2,
        2,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1
      ),
      label=c(
        "data scientist",
        "data scientist",
        "Human resource managers",
        "Human resource managers",
        "computer vision engineer",
        "computer vision engineer",
        "data scientist",
        "bioinformatics scientist",
        "bioinformatics scientist",
        "speech and language therapist",
        "speech and language therapist",
        "talent acquisition manager"
      )
    ))
    
    
    mat_fun=trend_anal_fun(data = data,features_query = "occupations","what"="year")
    
    mat_real$occupations$date=factor(mat_real$occupations$date,levels = levels(mat_fun$occupations$date))
    mat_real$occupations$item=factor(mat_real$occupations$item,levels = levels(mat_fun$occupations$item))
    rownames(mat_fun$occupations)=  rownames(mat_real$occupations)= NULL
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
  
  #countries - months
  {
    
    mat_real=list(country=data.frame(
      date=c("05","01","05","02"),
      item=c("Greece","Germany","Germany","Greece"),
      Freq=c(2,1,1,1)
    ))
    
    
    mat_fun=trend_anal_fun(data = data,features_query = "country","what"="month")
    
    mat_real$country$date=factor(mat_real$country$date,levels = levels(mat_fun$country$date))
    mat_real$country$item=factor(mat_real$country$item,levels = levels(mat_fun$country$item))
    rownames(mat_fun$country)=  rownames(mat_real$country)= NULL
    
    expect_equal(mat_fun,expected = mat_real) 
    
  }
  
  
  #countries - year_months
  {
    
    mat_real=list(country=data.frame(
      date=c("2022-01","2024-05","2024-05","2025-02","2025-05"),
      item=c("Germany","Germany","Greece","Greece","Greece"),
      Freq=c(1,1,1,1,1)
    ))
    
    
    mat_fun=trend_anal_fun(data = data,features_query = "country","what"="year_month")
    
    mat_real$country$date=factor(mat_real$country$date,levels = levels(mat_fun$country$date))
    mat_real$country$item=factor(mat_real$country$item,levels = levels(mat_fun$country$item))
    rownames(mat_fun$country)=  rownames(mat_real$country)= NULL
    
    expect_equal(mat_fun,expected = mat_real) 
    
  }
  
  
})


test_that("trend_anal_fun_prop works",{
  
  
  #occupations - year
  {
    mat_real=data.frame(
      Var1=c(
        "http://data.europa.eu/esco/isco/C2511",
        "http://data.europa.eu/esco/isco/C2511",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/isco/C2511",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/isco/C2131",
        "http://data.europa.eu/esco/isco/C2266",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/isco/C2131",
        "http://data.europa.eu/esco/isco/C2266"
      ),
      Var2=c(
        "2024",
        "2025",
        "2022",
        "2022",
        "2024",
        "2024",
        "2024",
        "2025",
        "2025",
        "2025"
      ),
      Freq=c(
        2,2,1,1,1,1,1,1,1,1
      ),
      label=c(
       "Systems analysts",
       "Systems analysts",
       "Human resource managers",
       "Systems analysts",
       "Human resource managers",
       "Biologists, botanists, zoologists and related professionals",
       "Audiologists and speech therapists",
       "Human resource managers",
       "Biologists, botanists, zoologists and related professionals",
       "Audiologists and speech therapists"
       
       )
    )
    
    
    mat_fun=trend_anal_fun_prop(data = data,features_query = "occupations",level = "3",what = "year")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    
    
    rownames(mat_real)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
    
  }
  
  #Skills (skill) - year
  {
    
    mat_real=data.frame(
      Var1=c(
        "http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943",
        "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",
        "http://data.europa.eu/esco/skill/28d2ab6c-38a2-476e-85fe-7982fd242943",
        "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",
        "http://data.europa.eu/esco/skill/7a00c77c-4a1c-4fd6-a83f-004bda65d81a",
        "http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80",
        "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9",
        "http://data.europa.eu/esco/skill/3753e796-121e-46a3-9c27-dee31ce92c80",
        "http://data.europa.eu/esco/skill/cacc62f3-2df4-4cc3-9d5d-0d014db56bd9"
        
        ),
      Var2=c(
        "2024",
        "2024",
        "2025",
        "2025",
        "2022",
        "2024",
        "2024",
        "2025",
        "2025"
      ),
      Freq=c(
        2,2,2,2,1,1,1,1,1
      ),
      label=c(
        "accessing and analysing digital data",
        "supervising people",
        "accessing and analysing digital data",
        "supervising people",
        "supervising people",
        "analysing and evaluating information and data",
        "using digital tools for collaboration, content creation and problem solving",
        "analysing and evaluating information and data",
        "using digital tools for collaboration, content creation and problem solving"
      )
      
    )
    
    mat_fun=trend_anal_fun_prop(data = data,features_query = "skills",pillar = "Skill",level = "2",what = "year")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    
    
    rownames(mat_real)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
  #Skills (knowledge and language) - year
  {
    
    mat_real=data.frame(
      Var1=c(
        "http://data.europa.eu/esco/isced-f/054",
        "http://data.europa.eu/esco/isced-f/061",
        
        "http://data.europa.eu/esco/isced-f/054",
        "http://data.europa.eu/esco/isced-f/061",
        
        "http://data.europa.eu/esco/isced-f/054",
        "http://data.europa.eu/esco/isced-f/061",
        
        "http://data.europa.eu/esco/isced-f/091",
        "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0",
        
        "http://data.europa.eu/esco/isced-f/091",
        "http://data.europa.eu/esco/skill/6d3edede-8951-4621-a835-e04323300fa0"
      ),
      Var2=c(
        "2024",
        "2024",
        
        "2025",
        "2025",
        
        "2022",
        "2022",
        
        "2024",
        "2024",
        
        "2025",
        "2025"
      ),
      Freq=c(
        2,
        2,
        
        2,
        2,
        
        1,
        1,
        
        1,
        1,
        
        1,
        1
      ),
      label=c(
        "mathematics and statistics",
        "information and communication technologies (icts)",
      
        "mathematics and statistics",
        "information and communication technologies (icts)",
        
        "mathematics and statistics",
        "information and communication technologies (icts)",
        
        "health",
        "English",
        
        "health",
        "English"
        )
    )
    
    
    mat_fun=trend_anal_fun_prop(data = data,features_query = "skills",pillar = "Knowledge;;Language",level = "2;;2",what = "year")
    
    mat_real$Var1=factor(mat_real$Var1,levels = levels(mat_fun$Var1))
    mat_real$Var2=factor(mat_real$Var2,levels = levels(mat_fun$Var2))
    
    
    rownames(mat_real)=rownames(mat_fun)=NULL
    
    expect_equal(mat_fun,expected = mat_real)
    
    
  }
  
  
})


test_that("unanticipated_freq_fun works",{
  
  #1 high
  {
    
   mat_real=20
   
   mat_fun=unanticipated_freq_fun(freq_in = 2,freq_all = 3,no_documents_query = 10,no_documents_all = 110)
   
   expect_equal(mat_fun,expected = mat_real)
   
  }
  
  #2 low
  {
    
    mat_real=0.5
    mat_fun=unanticipated_freq_fun(freq_in = 1,freq_all = 21,no_documents_query = 10,no_documents_all = 110)
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  #3 zero
  {
    mat_real=00
    mat_fun=unanticipated_freq_fun(freq_in = 0,freq_all = 21,no_documents_query = 10,no_documents_all = 110)
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  #4 Inifnite
  
  {
    mat_real=(1/10)/((.Machine$double.eps)/(110-10+.Machine$double.eps))
  
    
    mat_fun=unanticipated_freq_fun(freq_in = 1,freq_all = 1,no_documents_query = 10,no_documents_all = 110)
    expect_equal(mat_fun,expected = mat_real)
    
  }
  
  
})


test_that("unanticipated_freq_multi_fun_one works",{
  
  #1 Occupations - After 2024
  {
    
    mat_real=list(occupations=data.frame(
      Item=c(
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",
        "http://data.europa.eu/esco/isco/C1212",
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",#speech and language therapist
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75"
      ),
      Freq_in=c(
        2,
        1,
        1,
        1,
        1,
        0
        
      ),
      Freq_all=c(
        5,
        2,
        2,
        2,
        2,
        1
      ),
      Score=c(
        1,
        1.5,
        1.5,
        1.5,
        1.5,
        0
      )
    ),
    count_query=2,
    count_all=5
    )
    
    counter_all=data$count
    pos_now=c(1,3)
    counter_now=length(pos_now)
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    data_query_res=analytics_fun(features_query = "occupations",data = data_temp)
    data_all_res=analytics_fun(features_query = "occupations",data = data)
    
    mat_fun=unanticipated_freq_multi_fun_one(data_query_res,data_all_res,counter_now,counter_all)
    
    
    expect_equal(mat_fun,expected = mat_real)
  }
  
  
  #2 Countries - In 2024
  {
    mat_real=list(country=data.frame(
      Item=c(
        "Germany",
        "Greece"
         ),
      Freq_in=c(
        1,
        1
        
      ),
      Freq_all=c(
        2,
        3
      ),
      Score=c(
        1.5,
        0.75
      )
    ),
    count_query=2,
    count_all=5

    )
    
    
    counter_all=data$count
    pos_now=c(2,4)
    counter_now=length(pos_now)
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    data_query_res=analytics_fun(features_query = "country",data = data_temp)
    data_all_res=analytics_fun(features_query = "country",data = data)
    
    mat_fun=unanticipated_freq_multi_fun_one(data_query_res,data_all_res,counter_now,counter_all)
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001)
  }
  
  
  #3 Score - Germany
  {
    
    mat_real=list(score=data.frame(
      Item=c(
        '1',
        '5',
        '4',
        '2'
      ),
      Freq_in=c(
        1,
        1,
        0,
        0
      ),
      Freq_all=c(
        1,
        1,
        2,
        1

      ),
      Score=c(
        (1/2)/((.Machine$double.eps)/(5-2+.Machine$double.eps)),
        (1/2)/((.Machine$double.eps)/(5-2+.Machine$double.eps)),
        0,
        0
      )
    ),
    count_query=2,
    count_all=5
    
    )
    
    
    counter_all=data$count
    pos_now=c(2,5)
    counter_now=length(pos_now)
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    data_query_res=analytics_fun(features_query = "score",data = data_temp)
    data_all_res=analytics_fun(features_query = "score",data = data)
    
    mat_fun=unanticipated_freq_multi_fun_one(data_query_res,data_all_res,counter_now,counter_all)
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001)
    
  }
  
  
})


test_that("unanticipated_freq_multi_fun_double works",{
  
  #Country Occupations - score (more than 2)
  {
    
    mat_real=list(
      "country;;occupations"=data.frame(Item_1=c(
        "Greece",
        "Germany",
        "Greece",
        "Greece",
        "Greece",
        "Germany",
        "Greece",
        "Germany",
        "Germany"
        
      ),
      Item_2=c(
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",#speech and language therapist
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75" #talent acquisition manager
        
      ),
      Freq_in=c(
        2,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        0
        
      ),
      Freq_all=c(
        3,
        2,
        2,
        2,
        1,
        1,
        1,
        1,
 
        1
      ),
      Score=c(
        (2/3)/(1/2),
        (1/3)/(1/2),
        (1/3)/(1/2),
        (1/3)/(1/2),
        (1/3)/((0+.Machine$double.eps)/2),
        (1/3)/((0+.Machine$double.eps)/2),
        (1/3)/((0+.Machine$double.eps)/2),
        (1/3)/((0+.Machine$double.eps)/2),
 
        ((0+.Machine$double.eps)/3)/(1/2)

      )
      ),
      count_query=3,
      count_all=5
    )
    
    
    counter_all=data$count
    pos_now=c(1,2,4)
    counter_now=length(pos_now)
    features_query="country;;occupations"
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    data_query_res=list()
    data_query_res[[features_query]]=exploratory_fun(features_query = features_query,data = data_temp)
    
    data_all_res=list()
    data_all_res[[features_query]]=exploratory_fun(features_query = features_query,data = data)
    
    
    mat_fun=unanticipated_freq_multi_fun_double(data_query_res,data_all_res,counter_now,counter_all)
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001)
    
  }
  
  
  #Occupations  Occupations - score (more than 2)
  {
    mat_real=list(
      "occupations;;occupations"=data.frame(Item_1=c(
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30"#data scientist
        

      ),
      Item_2=c(
        "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
        "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",#speech and language therapist
        "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",#speech and language therapist
        "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75" #talent acquisition manager
        
      ),
      Freq_in=c(
        2,
        2,
        2,
        1,
        1,
        1,
        0
        
      ),
      Freq_all=c(
        2,
        2,
        2,
        2,
        2,
        2,
        1
      ),
      Score=c(
        (2/3)/((0+.Machine$double.eps)/2),
        (2/3)/((0+.Machine$double.eps)/2),
        (2/3)/((0+.Machine$double.eps)/2),
        (1/3)/(1/2),
        (1/3)/(1/2),
        (1/3)/(1/2),
        ((0+.Machine$double.eps)/3)/(1/2)
        
      )
      ),
      count_query=3,
      count_all=5
    )
    
    
    counter_all=data$count
    pos_now=c(1,2,4)
    counter_now=length(pos_now)
    features_query="occupations;;occupations"
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    data_query_res=list()
    data_query_res[[features_query]]=exploratory_fun(features_query = features_query,data = data_temp)
    
    data_all_res=list()
    data_all_res[[features_query]]=exploratory_fun(features_query = features_query,data = data)
    
    
    mat_fun=unanticipated_freq_multi_fun_double(data_query_res,data_all_res,counter_now,counter_all)
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001)
    
  }
  
  
  #Occupations-prop Country - score (more than 2)
  {
    
    mat_real=list("occupations;;country"=data.frame(
      Item_1=c(
        "http://data.europa.eu/esco/isco/C2511",#Systems analysts
        "http://data.europa.eu/esco/isco/C1212",#Human resource managers
        "http://data.europa.eu/esco/isco/C2511",#Systems analysts
        "http://data.europa.eu/esco/isco/C2131",#Biologists, botanists, zoologists and related professionals
        "http://data.europa.eu/esco/isco/C2266",#Audiologists and speech therapists (speech and language therapist)
        "http://data.europa.eu/esco/isco/C1212" #Human resource managers
        
        
        
      ),
      Item_2=c(
        "Greece",
        "Germany",
        "Germany",
        "Greece",
        "Greece",
        "Greece"
        
      ),
      Freq_in=c(
        2,
        1,
        1,
        1,
        1,
        1
        
      ),
      Freq_all=c(
        3,
        2,
        2,
        2,
        2,
        1
        
      ),
      Score=c(
        (2/3)/((1)/2),
        (1/3)/((1)/2),
        (1/3)/((1)/2),
        (1/3)/((1)/2),
        (1/3)/((1)/2),
        (1/3)/((0+.Machine$double.eps)/2)
        
      )
    ),
    count_query=3,
    count_all=5
)
    
    
    counter_all=data$count
    pos_now=c(1,2,4)
    counter_now=length(pos_now)
    features_query="occupations;;country"
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    data_query_res=list()
    data_query_res[[features_query]]=exploratory_fun_prop(features_query = features_query,data = data_temp,level_1 = 3)
    
    data_all_res=list()
    data_all_res[[features_query]]=exploratory_fun_prop(features_query = features_query,data = data,level_1 = 3)
    
    
    mat_fun=unanticipated_freq_multi_fun_double(data_query_res,data_all_res,counter_now,counter_all)
    
  
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001)
    
  }
  
})


test_that("frequencies_filtered_call_superfun works",{
  
  #Occupations propagation - ONE
  {
    mat_real=list(
      occupations=data.frame(
        Item=c(
        "http://data.europa.eu/esco/isco/C1212", #Human resource managers
        "http://data.europa.eu/esco/isco/C2511", #Systems analysts
        "http://data.europa.eu/esco/isco/C2131", #Biologists, botanists, zoologists and related professionals
        "http://data.europa.eu/esco/isco/C2266"  #Audiologists and speech therapists
        ),
        Freq_in=c(
          3,
          3,
          0,
          0
        ),
        Freq_all=c(
          3,
          5,
          2,
          2
        ),
        Score=c(
          (3/3)/((0+.Machine$double.eps)/2),
          1,
          0,
          0
        )
        ),
      count_query=3,
      count_all=5
    )
    
    mat_fun<-frequencies_filtered_call_superfun(
      data = data,target_feature = "country;;occupations",target_values = "Germany;;http://data.europa.eu/esco/isco/C1212",
      features_query = "occupations",level_1 = "3",type_analysis = "ONE",type_query = "OR",unanticipated_freq_arg = "YES")
  
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001) 
    
  }
  
  
  #Occupations - ONE
  {
    
    mat_real=list(
      occupations=data.frame(
        Item=c(
          "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
          "http://data.europa.eu/esco/isco/C1212",#Human resource managers
          "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
          "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75",#talent acquisition manager
          "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
          "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7" #speech and language therapist
          
          ),
        Freq_in=c(
          3,
          2,
          2,
          1,
          0,
          0
        ),
        Freq_all=c(
          5,
          2,
          2,
          1,
          2,
          2
        ),
        Score=c(
          (3/3)/((2+.Machine$double.eps)/2),
          (2/3)/((0+.Machine$double.eps)/2),
          (2/3)/((0+.Machine$double.eps)/2),
          (1/3)/((0+.Machine$double.eps)/2),
          (0/3)/((2+.Machine$double.eps)/2),
          (0/3)/((2+.Machine$double.eps)/2)
          
        )
      ),
      count_query=3,
      count_all=5
    )
    
    mat_fun<-frequencies_filtered_call_superfun(
      data = data,target_feature = "country;;occupations",target_values = "Germany;;http://data.europa.eu/esco/isco/C1212",
      features_query = "occupations",type_analysis = "ONE",type_query = "OR",unanticipated_freq_arg = "YES")
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001) 
    
  }
  
  
  #Occupations propagation and country - Pair
  {
    
    mat_real=list(
      "occupations;;country"=data.frame(
        Item_1=c(
        "http://data.europa.eu/esco/isco/C2511", #Systems analysts
        "http://data.europa.eu/esco/isco/C1212", #Human resource managers
        "http://data.europa.eu/esco/isco/C2511", #Systems analysts
        "http://data.europa.eu/esco/isco/C2131", #Biologists, botanists, zoologists and related professionals
        "http://data.europa.eu/esco/isco/C2266", #Audiologists and speech therapists
        "http://data.europa.eu/esco/isco/C1212"  #Human resource managers
        ),
        Item_2=c(
          "Greece",
          "Germany",
          "Germany",
          "Greece",
          "Greece",
          "Greece"
        ),
        Freq_in=c(
          3,
          1,
          1,
          2,
          2,
          1
        ),
        Freq_all=c(
          3,
          2,
          2,
          2,
          2,
          1
        ),
        Score=c(
          (3/4)/((0+.Machine$double.eps)/1),
          (1/4)/((1+.Machine$double.eps)/1),
          (1/4)/((1+.Machine$double.eps)/1),
          (2/4)/((0+.Machine$double.eps)/1),
          (2/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1)
          
        )
      ),
      count_query=4,
      count_all=5
    )
    
    mat_fun<-frequencies_filtered_call_superfun(
      data = data,target_feature = "skills",target_values ="http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis


      features_query = "occupations;;country",level_1 = "3",type_analysis = "PAIR",type_query = "OR",unanticipated_freq_arg = "YES")
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001) 
    
  }
  
  
  #Occupations and country - Pair
  {
    
    mat_real=list(
      "occupations;;country"=data.frame(
        Item_1=c(
          "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
          "http://data.europa.eu/esco/occupation/68d973df-bf10-4bf7-9a1b-fbd9f604b9db",#bioinformatics scientist
          "http://data.europa.eu/esco/occupation/8021f3a2-e3de-43c0-b366-075da74dc5b7",#speech and language therapist
          "http://data.europa.eu/esco/occupation/258e46f9-0075-4a2e-adae-1ff0477e0f30",#data scientist
          "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
          "http://data.europa.eu/esco/isco/C1212",#Human resource managers
          "http://data.europa.eu/esco/occupation/1c5a45b9-440e-4726-b565-16a952abd341",#computer vision engineer
          "http://data.europa.eu/esco/isco/C1212",#Human resource managers
          "http://data.europa.eu/esco/occupation/a6e8a6bf-9209-4d19-a131-a8b6431e0f75" #talent acquisition manager
        ),
        Item_2=c(
          "Greece",
          "Greece",
          "Greece",
          "Germany",
          "Greece",
          "Greece",
          "Germany",
          "Germany",
          "Germany"
        ),
        Freq_in=c(
          3,
          2,
          2,
          1,
          1,
          1,
          1,
          1,
          0
        ),
        Freq_all=c(
          3,
          2,
          2,
          2,
          1,
          1,
          1,
          1,
          1
        ),
        Score=c(
          (3/4)/((0+.Machine$double.eps)/1),
          (2/4)/((0+.Machine$double.eps)/1),
          (2/4)/((0+.Machine$double.eps)/1),
          (1/4)/((1+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (0/4)/((1+.Machine$double.eps)/1)
          
        )
      ),
      count_query=4,
      count_all=5
    )
    
    mat_fun<-frequencies_filtered_call_superfun(
      data = data,target_feature = "skills",target_values ="http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
      
      
      features_query = "occupations;;country",type_analysis = "PAIR",type_query = "OR",unanticipated_freq_arg = "YES")
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001) 
    
    
  }

  
  #Country trend - skill filter
  {
    
    mat_real=list(
     country=data.frame(
      Item_1=c(
        "2025",
        "2022",
        "2024",
        "2024"
      ),
      Item_2=c(
        "Greece",
        "Germany",
        "Germany",
        "Greece"
      ),
      Freq_in=c(
        2,
        0,
        1,
        1
      ),
      Freq_all=c(
        2,
        1,
        1,
        1
      ),
      Score=c(
        (2/4)/((0+.Machine$double.eps)/1),
        (0/4)/((1+.Machine$double.eps)/1),
        (1/4)/((0+.Machine$double.eps)/1),
        (1/4)/((0+.Machine$double.eps)/1)
        
      )
    ),
    count_query=4,
    count_all=5
    )
    
    
    mat_fun<-frequencies_filtered_call_superfun(
      data = data,target_feature = "skills",target_values ="http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
      features_query = "country",type_analysis = "TREND-year",date_field = "upload_date",
      type_query = "OR",unanticipated_freq_arg = "YES")
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001) 
    
  }
  
  #Occupations prop trend - skill filter
  {
    
    mat_real=list(
      occupations=data.frame(
        Item_1=c(
          "http://data.europa.eu/esco/isco/C2511", #Systems analysts
          "http://data.europa.eu/esco/isco/C2511", #Systems analysts
          "http://data.europa.eu/esco/isco/C1212", #Human resource managers
          "http://data.europa.eu/esco/isco/C2511", #Systems analysts
          "http://data.europa.eu/esco/isco/C1212", #Human resource managers
          "http://data.europa.eu/esco/isco/C2131", #Biologists, botanists, zoologists and related professionals
          "http://data.europa.eu/esco/isco/C2266", #Audiologists and speech therapists
          "http://data.europa.eu/esco/isco/C1212", #Human resource managers
          "http://data.europa.eu/esco/isco/C2131", #Biologists, botanists, zoologists and related professionals
          "http://data.europa.eu/esco/isco/C2266"  #Audiologists and speech therapists
        ),
        Item_2=c(
          "2024",
          "2025",
          "2022",
          "2022",
          "2024",
          "2024",
          "2024",
          "2025",
          "2025",
          "2025"
        ),

        Freq_in=c(
          2,
          2,
          0,
          0,
          1,
          1,
          1,
          1,
          1,
          1
        ),
        Freq_all=c(
          2,
          2,
          1,
          1,
          1,
          1,
          1,
          1,
          1,
          1
        ),
        Score=c(
          (2/4)/((0+.Machine$double.eps)/1),
          (2/4)/((0+.Machine$double.eps)/1),
          (0/4)/((1+.Machine$double.eps)/1),
          (0/4)/((1+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1),
          (1/4)/((0+.Machine$double.eps)/1)
        )
      ),
      count_query=4,
      count_all=5
    )
    
    
    mat_fun<-frequencies_filtered_call_superfun(
      data = data,target_feature = "skills",target_values ="http://data.europa.eu/esco/skill/2b92a5b2-6758-4ee3-9fb4-b6387a55cc8f",#perform data analysis
      features_query = "occupations",level_1 = "3",type_analysis = "TREND-year",date_field = "upload_date",
      type_query = "OR",unanticipated_freq_arg = "YES")
    
    
    expect_equal(mat_fun,expected = mat_real,tolerance = 0.00001) 
    
    
  }
  
})



