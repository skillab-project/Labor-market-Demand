
#path_user_files_all="C:/Users/kostas charm/Documents/Skillab/analytics_microservice/"
#path_user_files_all="C:/Users/zapfl/OneDrive/Documents/phd/Skillab/analytics_microservice/"
path_user_files_all="~/user_sessions/"







load_now_condition=T

library(dotenv);dotenv::load_dot_env(file = ".env")
library(plumber)
library(future)
library(promises)
library(igraph)
library(ggplot2);library(plotly);library(ggthemes)
library(httr)
library(jsonlite)
library(dplyr)
library(apcluster)
library(uwot)
library(text2vec)
library(cluster)
library(mclust) 
library(factoextra)
#library(ggrepel)
library(ggforce)
library(binda)
library(FactoMineR)



plan(multisession)  # Enable multi-threading

###### plumber.R


###Get token
get_valid_token <- function(API_BASE_URL,USERNAME,PASSWORD) {
  response <- POST(
    paste0(API_BASE_URL, "/login"),
    body = list(username = USERNAME, password = PASSWORD),
    encode = "json",
    add_headers(
      "accept" = "application/json",
      "Content-Type" = "application/json"
    ),
    verbose()  # Debug authentication
  )
  
  if (status_code(response) == 200) {
    token <- gsub('"', "", content(response, "text"))
    cat("Token acquired:", substr(token, 1, 20), "...\n")  # Log partial token
    return(token)
  } else {
    stop(sprintf("Auth failed (Status %d): %s", 
                 status_code(response),
                 content(response, "text")))
  }
}



###API example one page
api_ex_now_one<-function(url,body=list()){
  
  #API default
  #https://skillab-tracker.csd.auth.gr/api/docs#/Job/api_views_get_jobs
  
  #Define the API endpoint and parameters
  #url <- "https://skillab-tracker.csd.auth.gr/api/jobs?page=1"
  #url <- "https://skillab-tracker.csd.auth.gr/api/law-publications?page=1"
  
  #Define the POST request body
  #body <- 'keywords=seller&keywords=market'
  #body <- list(
  #keywords='seller',
  #keywords='market'
  #)
  
  # Load the libraries

  
  
  # Make the POST request
  response <- POST(
    url,
    add_headers(
      `accept` = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    ),
    body = body,
    encode = "form"
  )
  
  # Parse the JSON response into a list
  if (http_type(response) == "application/json") {
    
    items <- as.list(content(response, "parsed"))
    return(items)
    
    
  } else {
    print("Response is not in JSON format.")
  }
  
  
}


###API example all pages
api_ex_now<-function(url,body=list(),per_page=100,limit_data_no=""){
  
  
  API_BASE_URL=Sys.getenv("API_BASE_URL")
  USERNAME=Sys.getenv("USERNAME")
  PASSWORD=Sys.getenv("PASSWORD")
  
  bearer_token=get_valid_token(API_BASE_URL,USERNAME,PASSWORD)
  
  #url="https://skillab-tracker.csd.auth.gr/api/skills"
  
  #url <- "https://skillab-tracker.csd.auth.gr/api/jobs"
  #body <- 'keywords=seller&keywords=market'
  #per_page <- 300
  
  #Exmaples body and url
  #url=paste0("https://skillab-tracker.csd.auth.gr/api/skills",page)
  #body=list(min_skill_level=1) OR body="min_skill_level=1"
  
  #Define parameters
  page=1
  

  # Load the libraries

  
  
  url_now=paste0(url,"?page=",page,"&page_size=",per_page)
  #url_now=paste0(url)
  
  # Make the POST request
  response <- POST(
    url_now,
    add_headers(
      `accept` = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded",
      `Authorization` = paste("Bearer",bearer_token)
      
    ),
    body = body,
    encode = "form"
  )
  
  
  
  items <- as.list(content(response, "parsed"))
  print("PAGE 1")
  
  items$endpoint=url_now
  items$query=body
  
  if(items$count>0 & nchar(limit_data_no)>0){
    limit_data_no=as.numeric(limit_data_no)
    items$count=limit_data_no
  }
  
  if(items$count>0&items$count>=per_page){
    
    
    pages_to_collect=items$count%/%per_page
    mod_now=items$count %%per_page
    if (mod_now!=0)pages_to_collect=pages_to_collect+1
    
    for (page in 2:pages_to_collect){
      
      url_now=paste0(url,"?page=",page,"&page_size=",per_page)
      #url_now=paste0(url,"?offset=",length(items$items))
      
      
      print(paste("PAGE",page))
      # Make the POST request
      response <- POST(
        url_now,
        add_headers(
          `accept` = "application/json",
          `Content-Type` = "application/x-www-form-urlencoded",
          `Authorization` = paste("Bearer",bearer_token)
        ),
        body = body,
        encode = "form"
      )
      
      items$items <- c(items$items,as.list(content(response, "parsed"))$items)

    }
    return(items)
    
  }else if (items$count<=per_page){
    return(items)
  }
  else{
    return(list("ERROR"="0 items found"))
  }
  

  
  
}



if(load_now_condition){
  
  # Flatten and normalize the list for consistent rows in skills
  flatten_list_skills <- function(item) {
    # Ensure all elements are atomic vectors or single values
    list(
      
      id = item$id,
      label = item$label,
      alternative_labels = item$alternative_labels,
      description = item$description,
      
      skill_ancestors = item$skill_ancestors,
      knowledge_ancestors = item$knowledge_ancestors,
      traversal_ancestors = item$traversal_ancestors,
      language_ancestors =  item$language_ancestors,
      
      skill_levels = item$skill_levels,
      traversal_levels = item$traversal_levels,
      knowledge_levels = item$knowledge_levels,
      language_levels = item$language_levels,
      
      
      children = item$children
      
    )
  }
  
  
  ###Get all skills
  url="https://skillab-tracker.csd.auth.gr/api/skills"
  #body="min_skill_level=3"
  print("Loading Skill Data")
  data_all_skills=api_ex_now(url)
  data_all_skills <- do.call(rbind, lapply(data_all_skills$items, flatten_list_skills))
  data_all_skills <- as.data.frame(data_all_skills, stringsAsFactors = FALSE)
  
  gc()
  
  
  # Flatten and normalize the list for consistent rows in skills
  flatten_list_occupations <- function(item) {
    # Ensure all elements are atomic vectors or single values
    list(
      
      id = item$id,
      label = item$label,
      alternative_labels = item$alternative_labels,
      description = item$description,
      
      ancestors = item$ancestors,
      
      
      levels = item$levels,
      
      
      
      children = item$children
      
    )
  }
  
  ###Get all occupations
  url="https://skillab-tracker.csd.auth.gr/api/occupations"
  #body="min_level=1"
  
  print("Loading Occupation Data")
  data_all_occupations=api_ex_now(url)
  data_all_occupations <- do.call(rbind, lapply(data_all_occupations$items, flatten_list_occupations))
  data_all_occupations <- as.data.frame(data_all_occupations, stringsAsFactors = FALSE)
  
  gc()
  
}


#data<-NULL


###Occurence Matrix from single values per observation
occur_mat_fun<-function(data_field,search_field){
  
  table_field=table(data_field[,search_field])
  
  mat_field=matrix(nrow = nrow(data_field),ncol=length(table_field),0)
  colnames(mat_field)=names(table_field)
  
  match_field=match(unlist(data_field[,search_field]),names(table_field))
  
  for(i in 1:nrow(data_field)) mat_field[i,match_field[i]]=1
  
  return(mat_field)
}

####Occurence Matrix from doubles of skills (or anything else)
double_to_occur<-function(data){

  
  
  
  
  # Data as factor
  data[, 1] <- as.factor(unlist(data[,1]))
  data[, 2] <- as.factor(unlist(data[, 2]))
  gc()
  
  #Get all possible factors
  all_rows=levels(data[[colnames(data)[1]]])
  all_cols=levels(data[[colnames(data)[2]]])
  gc()
  
  #Data to integers and as matrix
  data[, 1] <- as.integer(unlist(data[,1]))
  data[, 2] <- as.integer(unlist(data[, 2]))
  data=as.matrix(data)
  
  gc()
  
  binary_matrix=matrix(0,nrow=length(all_rows),ncol = length(all_cols),dimnames = list(all_rows,all_cols))
  
  remove(all_rows,all_cols)
  gc()
  
  for (i in 1:nrow(data)){
    #print(i)
    
    binary_matrix[as.integer(data[i,1]),as.integer(data[i,2])]=1
    
  }
  
  
  return(binary_matrix)
  #return(list(cooccurrence_matrix))
}


####Occurence Matrix from doubles of skills (or anything else) to ancestors (table_now)
double_to_occur_with_propagate<-function(data,table_now,is_freq=F){
  
  data=data[unlist(data[,2])%in%unique(unlist(table_now[,1])),]
  
  
  # Data as factor
  data[, 1] <- as.factor(unlist(data[,1]))
  data[, 2] <- as.factor(unlist(data[, 2]))
  gc()
  
  
  table_now[,1]=factor(unlist(table_now[,1]),levels = levels(data[,2]))
  table_now[,2]=as.factor(unlist(table_now[,2]))
  gc()
  
  #Get all possible factors
  all_rows=levels(data[[colnames(data)[1]]])
  all_cols=levels(data[[colnames(data)[2]]])
  gc()
  
  
  all_ancestors=levels(table_now[[colnames(table_now)[2]]])
  gc()
  
  
  #Data to integers and as matrix
  data[, 1] <- as.integer(unlist(data[,1]))
  data[, 2] <- as.integer(unlist(data[, 2]))
  data=as.matrix(data)
  gc()
  
  table_now[,1]=as.integer(unlist(table_now[,1]))
  table_now[,2]=as.integer(unlist(table_now[,2]))
  table_now=as.matrix(table_now)
  gc()
  
  binary_matrix=matrix(0,nrow=length(all_rows),ncol = length(all_ancestors),dimnames = list(all_rows,all_ancestors))
  
  
  if(is_freq){
    for(i in 1:nrow(data)){
      
      #print(i)
      
      item_pos=data[i,1]
      skill_pos=data[i,2]
      
      new_pos=as.numeric(which(table_now[,1]==skill_pos))
      new_pos=table_now[new_pos,2]
      
      binary_matrix[item_pos,new_pos]=binary_matrix[item_pos,new_pos]+1
      
    }
  }else{
    for(i in 1:nrow(data)){
      
      #print(i)
      
      item_pos=data[i,1]
      skill_pos=data[i,2]
      
      new_pos=as.numeric(which(table_now[,1]==skill_pos))
      new_pos=table_now[new_pos,2]
      
      binary_matrix[item_pos,new_pos]=1
      
    }
  }
 
  
  return(binary_matrix)
  
}

####Double from doubles of skills (or anything else) to ancestors (table_now)
double_to_double_with_propagate<-function(data,table_now){
  
  data=data[unlist(data[,2])%in%unique(unlist(table_now[,1])),]
  
  colnames(data)[2]="link"
  colnames(table_now)[1]='link'
  
  new_double=merge(data, table_now, by = "link")
  new_double=new_double[,-1]
  new_double=unique(new_double)
  
  return(new_double)
  
}


###Propagation Skills
propagation_skills <-function(data,urls,pillar="Skill;;Language",level="1;;2"){
  
  urls=unique(urls)

  pillar_split=unlist(strsplit(pillar,";;"))
  level_split=as.numeric(unlist(strsplit(level, ";;")))
  column_levels=c()
  column_ancestors=c()
  
  
  if("Skill"%in%pillar_split){
    column_levels=c(column_levels,"skill_levels")
    column_ancestors=c(column_ancestors,"skill_ancestors")
  }
  if("Knowledge"%in%pillar_split){
    column_levels=c(column_levels,"knowledge_levels")
    column_ancestors=c(column_ancestors,"knowledge_ancestors")
  }
  if("Language"%in%pillar_split){
    column_levels=c(column_levels,"language_levels")
    column_ancestors=c(column_ancestors,"language_ancestors")
  }
  if("Traversal"%in%pillar_split){
    column_levels=c(column_levels,"traversal_levels")
    column_ancestors=c(column_ancestors,"traversal_ancestors")
  }
  
  
  l<-0
  
  #x=unlist(data_all_skills$id[14])

  link_skill=do.call(
    
    rbind,
    lapply(urls,function(x){
      
      l<<-l+1
      
      pos_now=match(x,data_all_skills$id)
      
      
      ancestors_now=c()
      #temp_ancestors=list()
      #temp_levels=list()
      
      for(t in 1:length(column_ancestors)){
        
        temp_levels=unlist(data_all_skills[[column_levels[t]]][pos_now][[1]])
        #temp_levels=unlist(c(temp_levels,unlist(data_all_skills[[column_levels[t]]][pos_now][[1]])))
        
        temp_ancestors=data_all_skills[[column_ancestors[t]]][pos_now][[1]]
        #temp_ancestors=c(temp_ancestors,data_all_skills[[column_ancestors[t]]][pos_now][[1]])
        
        if(length(temp_levels)!=0){
          
          if(any(temp_levels==level_split[t]))ancestors_now=c(ancestors_now,x)
          
          levels_which=which(unlist(temp_levels)>level_split[t])
          
          if(length(levels_which)!=0){
            
            
            ancestors_now=c(ancestors_now,unlist(lapply(temp_ancestors[levels_which],function(y){
              
              
              y=unlist(y)
              
              lvl_now=length(y)-level_split[t]
              
              return(y[[lvl_now]])
              
            }
            
            
            )
            )
            )
          }
          
          
        }
        
      }
      
      if(length(ancestors_now)==0){
        return(NULL)
      }else{
        return(data.frame(Original_id=x,Ancestors_now=ancestors_now))
        
      }
      
    }
    
    
    )
    
  )
  
  
  
  return(unique(link_skill))
  
}


###Propagation Occupations
propagation_occupations <-function(data,urls,level="1"){
  
  urls=unique(urls)
  
  level=as.numeric(level)
  
  column_levels="levels"
  column_ancestors="ancestors"
  
  
  l<-0
  
  link_occupation=do.call(
    
    rbind,
    lapply(urls,function(x){
      
      l<<-l+1
      
      
      
      pos_now=match(x,data_all_occupations$id)
      
      temp_levels=unlist(data_all_occupations[[column_levels]][pos_now][[1]])
      temp_ancestors=data_all_occupations[[column_ancestors]][pos_now][[1]]
      
      if (is.null(temp_levels)){
        temp_levels=0
        temp_ancestors=x
      }
      
      if(length(temp_levels)!=0){
        
        levels_which=which(unlist(temp_levels)>=level)
        
        if(length(levels_which)!=0){
          
          k<-0
          
          ancestors_now=unlist(lapply(temp_ancestors[levels_which],function(y){
            
            k<<-k+1
            if(temp_levels[levels_which[k]]==level){
              return(urls[l])
            }else{
              
              y=unlist(y)
              lvl_now=length(y)-level
              
              return(y[[lvl_now]])
              
            }
            
          }
          
          ))
          
          
          return(data.frame(Original_id=x,Ancestors_now=ancestors_now))
          
          
        }else{
          return(NULL)
        }
      }else{
        return(NULL)
      }
      
      
      
      
    })
  )
  
  
  
  return(unique(link_occupation))
  
}


###Return double to occur with propagation
return_double_to_occur_propagate<-function(data,what,pillar,level,is_mat=T,return_freq=F){
  
  data_now <- do.call(
    rbind,
    lapply(data$items, function(x) {
      if (length(x[[what]]) > 0 & !is.null(x[[what]])) {
        do.call(
          rbind,
          lapply(x[[what]], function(y) {
            return(data.frame(item_id = x$id, skill = y, stringsAsFactors = FALSE))
          })
        )
      }
    })
  )
  
  
  if(what=="occupations"){
    table_now=propagation_occupations(data,urls = unique(unlist(data_now[,2])),level = level)
    
  }else if (what=="skills"){
    table_now=propagation_skills(data,urls = unique(unlist(data_now[,2])),pillar = pillar,level = level)
    
  }
  
  if (is_mat){
    
    return(double_to_occur_with_propagate(data_now,table_now,is_freq = return_freq))
    
  }else{
    return(double_to_double_with_propagate(data_now,table_now))
    
  }
  
  

}


#####Co-occurence mat
co_occurence_mat<-function(matrix1,matrix2){
  return(t(matrix1) %*% matrix2)
}

#####Co occurence weights
co_occurence_weight<-function(data,weight="ii_weight",no_documents=0){
  diag_values=diag(data)
  
  if(weight=="ii_weight"){
    for (i in 1:(nrow(data)-1)){
      for (j in (i+1):nrow(data)){
        temp_val=min(data[i,i],data[j,j])
        data[i,j]=data[i,j]/temp_val
        data[j,i]=data[j,i]/temp_val
        
      }
    }
  }else if (weight=="ji_weight"){
    
    for (i in 1:(nrow(data)-1)){
      for (j in (i+1):nrow(data)){
        
        temp_val=(data[i,j])/(data[i,i]+data[j,j]-data[i,j])
        data[i,j]=temp_val
        data[j,i]=temp_val
        
      }
    }
    
  }else if (weight=="ei_weight"){
    
    for (i in 1:(nrow(data)-1)){
      for (j in (i+1):nrow(data)){
        
        temp_val=(data[i,j]*data[i,j])/(data[i,i]*data[j,j])
        data[i,j]=temp_val
        data[j,i]=temp_val
        
      }
    }
    
  }else if (weight=="mh_weight"){
    for (i in 1:(nrow(data)-1)){
      
      for (j in (i+1):nrow(data)){
        temp_val=log2((data[i,j]/no_documents)/(.Machine$double.xmin+data[i,i]/no_documents*data[j,j]/no_documents)+.Machine$double.xmin)
        data[i,j]=temp_val
        data[j,i]=temp_val
        
      }
    }
    
  }
  
  return(data)
  
  
}



#####Skill vectors
word_vectors_method<-function(input_mat,umap_nn=5,umap_dim=2,vectors_type="GloVe"){
  
  
  
  
  set.seed(123)
  
  if(vectors_type=="weighting"){
    
    word_vectors=umap(as.dist(1-input_mat),n_neighbors = umap_nn,n_components = umap_dim,verbose = T)
    
  }else{
    
    if(vectors_type=="GloVe"){
      
      lr=0.05 ; ct=-1
      
      glove = GloVe$new(rank = 50,x_max = 1000 ) #x_max = 10, ,learning_rate=lr
      
      set.seed(831)
      wv_main = glove$fit_transform(as.matrix(input_mat), convergence_tol = ct, n_threads = 4) #, n_iter = 100
      
      #
      wv_context = glove$components
      dim(wv_context)
      
      #
      new_vectors = wv_main + t(wv_context)
      
    }
    
    word_vectors=umap(new_vectors,metric = "cosine",n_neighbors = umap_nn,n_components = umap_dim,verbose = T)
    
  }
  
  rownames(word_vectors)=rownames(input_mat)
  #colnames(word_vectors)=c("x","y")
  
  word_vectors=as.data.frame(word_vectors)
  
  return(word_vectors)
  
  
}


####Clustering algorithms
leiden_clust_plot <- function(skill_co_mat){

  set.seed(831)
  no_edges=rowSums(skill_co_mat)
  zero_edges=which(no_edges==0)
  
  if(length(zero_edges)!=nrow(skill_co_mat)){
    ##igraph
    if(length(zero_edges)==0){
      g_final <-graph.adjacency(adjmatrix = skill_co_mat, mode="undirected") # For directed networks - graph_from_adjacency_matrix
      
    }else{
      g_final <-graph.adjacency(adjmatrix = skill_co_mat[-zero_edges,-zero_edges], mode="undirected") # For directed networks - graph_from_adjacency_matrix
      skill_co_mat <- skill_co_mat[-zero_edges,-zero_edges]
    }
    
    
    l_clust_final=cluster_leiden(graph = g_final,resolution_parameter = 1,objective_function = "modularity")
    l_clust_final$nb_clusters=length(sizes(l_clust_final))
    
    
    
    
    if(length(l_clust_final)!=0){
      
      token_memberships_all_final=as.numeric(membership(l_clust_final))
      coords <- layout.fruchterman.reingold(g_final)*0.5
      
      disc_nodes_pos=which(token_memberships_all_final==0)
      
      
      
      if(length(disc_nodes_pos)==0){
        
        all_df=cbind(coords,as.numeric(token_memberships_all_final))
        all_df=cbind(all_df,rownames(skill_co_mat))
        
      }else{
        
        all_df=cbind(coords,as.numeric(token_memberships_all_final[-disc_nodes_pos]))
        all_df=cbind(all_df,rownames(skill_co_mat)[-disc_nodes_pos])
        
      }
    }
    
    
    all_df=as.data.frame(all_df)
    
    all_df$V1=as.numeric(all_df$V1)
    all_df$V2=as.numeric(all_df$V2)
    all_df$V3=as.numeric(all_df$V3)
    
    
    colnames(all_df)=c("V1","V2","cluster","Label")
    

    g_matrix <- get.data.frame(g_final)
    
    g_matrix$from.x <- all_df$V1[match(g_matrix$from, all_df$Label)]  
    g_matrix$from.y <- all_df$V2[match(g_matrix$from, all_df$Label)]
    g_matrix$to.x <- all_df$V1[match(g_matrix$to, all_df$Label)]  
    g_matrix$to.y <- all_df$V2[match(g_matrix$to, all_df$Label)]
    
    

    
    all_df$cluster=as.character(all_df$cluster)
    

    
    return(list(all_df,g_matrix))
    
  }
}


affinity_clust_plot <-function(all_pairs_similarity,diag_values){

  set.seed(831)
  ap_clust=apcluster(all_pairs_similarity)
  
  clust=matrix(ncol=1,nrow = nrow(all_pairs_similarity),0)
  for(i in 1:length(ap_clust@clusters)){
    clust[ap_clust@clusters[[i]],1]=i
  }
  
  
  ###Points
  df=data.frame(Label=rownames(all_pairs_similarity),cluster=clust[,1])
  #df$freq=diag_values
  df$cluster=as.character(df$cluster)
  
  
  
  return(list(df,data.frame(Label=df$Label[ap_clust@exemplars],pos=ap_clust@exemplars)))
  
}


kmeans_clust_plot <-function(word_vectors,no_clust=10,diag_values,labeled_legend=F){
  

  
  
  set.seed(831)
  
  clust_now=kmeans(x = word_vectors,centers = no_clust)
  
  
  word_vectors$cluster=as.numeric(unlist(clust_now$cluster))
  word_vectors$Label=rownames(word_vectors)
  
  
  return(list(word_vectors))
  
}


gmm_clust_plot <-function(word_vectors,no_clust=10,diag_values,labeled_legend=F){
  
  set.seed(831)
  
  clust_now=Mclust(word_vectors,G=no_clust,verbose = TRUE) 
  word_vectors$cluster=as.numeric(unlist(clust_now$classification))
  word_vectors$Label=rownames(word_vectors)
  

  
  return(list(word_vectors))
  
}



######Custom queries within the data

query_pos_fun<-function(data,target_feature,target_value){
  
  #target_feature="skill_propagation"
  #target_value="http://data.europa.eu/esco/isced-f/06"
  
  #target_feature="occupation_propagation"
  #target_value="http://data.europa.eu/esco/isco/C12"  
  
  #data=data$items
  
  
  is_numeric_fun <- function(x) {
    if (is.null(x))return(FALSE)
    if (is.na(x)) return(FALSE)
    tryCatch({
      y=as.numeric(x)
      if(is.na(y))return(FALSE)
      TRUE
    }, warning = function(w) {
      FALSE
    }, error = function(e) {
      FALSE
    })
  }
  
  
  positions_query=c()
  
  if(substr(target_feature,1,2)=="!!"){
    
    if(is_numeric_fun(target_value))target_value=as.numeric(target_value)
    
    target_feature_new=substr(target_feature,6,nchar(target_feature))
    condition_new=substr(target_feature,3,4)
    
    if(condition_new=="EQ"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          if(temp_items==target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="GR"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          if(temp_items>target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="GE"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          if(temp_items>=target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="LO"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          if(temp_items<target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="LE"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          if(temp_items<=target_value)positions_query=c(positions_query,i)
        }
      }
    }
    
  }else if(substr(target_feature,1,2)=="!?"){
    
    #!?GEY_upload_date -> Y =year , M=month , B=year_month

    target_feature_new=substr(target_feature,7,nchar(target_feature))
    condition_new=substr(target_feature,3,4)
    
    
    if(substr(target_feature,5,5)=="Y"){
      value_char=c(1,4)
    }else if(substr(target_feature,5,5)=="M"){
      value_char=c(6,7)
    }else if (substr(target_feature,5,5)=="B"){
      value_char=c(1,7)
    }
    
    
    if(condition_new=="EQ"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          temp_items=substr(temp_items,value_char[1],value_char[2])
          if(temp_items==target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="GR"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          temp_items=substr(temp_items,value_char[1],value_char[2])
          if(temp_items>target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="GE"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          temp_items=substr(temp_items,value_char[1],value_char[2])
          if(temp_items>=target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="LO"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          temp_items=substr(temp_items,value_char[1],value_char[2])
          if(temp_items<target_value)positions_query=c(positions_query,i)
        }
      }
    }else if(condition_new=="LE"){
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature_new]])
        if(length(temp_items)>0){
          temp_items=substr(temp_items,value_char[1],value_char[2])
          if(temp_items<=target_value)positions_query=c(positions_query,i)
        }
      }
    }
    }else{
    if(target_feature=="skill_propagation"|target_feature=="occupation_propagation"){
      
      if(length(grep("skill_propagation",target_feature))>0){
        tf_now_temp="skills"
      }else{
        tf_now_temp="occupations"
      }
      
      data_now_list_target=list()
      
      for (i in 1:length(data$items)){
        
        if (length(data$items[[i]][[tf_now_temp]]) > 0 & !is.null(data$items[[i]][[tf_now_temp]]) ) {
          tf_list=unique(unlist((data$items[[i]][[tf_now_temp]])))
          for (tf_temp in tf_list){
            if(is.null(data_now_list_target[[tf_temp]])){
              data_now_list_target[[tf_temp]]=c(i)
            }else{
              data_now_list_target[[tf_temp]]=c(data_now_list_target[[tf_temp]],i)
            }
          }
        }
        
      }
      
      names_target=names(data_now_list_target)
      names_target_in=c()
      
      if(target_feature=="skill_propagation"){
        
        for (nt in names_target){
          i=match(nt,data_all_skills$id)
          temp_ancestors=unique(c(nt,unlist(data_all_skills[i,c('id',"skill_ancestors","knowledge_ancestors","traversal_ancestors","language_ancestors")])))
          
          #if(any(temp_ancestors==target_value)){
          if(any(temp_ancestors%in%target_value)){
            names_target_in=c(names_target_in,nt)
          }
          
        }
        
      }else if(target_feature=="occupation_propagation"){
        
        for (nt in names_target){
          i=match(nt,data_all_occupations$id)
          
          temp_ancestors=unique(c(nt,unlist(data_all_occupations[i,c('id',"ancestors")])))
          
          #if(any(temp_ancestors==target_value)){
          if(any(temp_ancestors%in%target_value)){
            names_target_in=c(names_target_in,nt)
          }
          
        }
      }
      
      if(length(names_target_in)>0){
        positions_query=c(positions_query,unique(unlist(data_now_list_target[names_target_in])))
      } 
      
      
    }else{
      
      for(i in 1:length(data$items)){
        temp_items=unlist(data$items[[i]][[target_feature]])
        if(length(temp_items)>0){
          #if(any(temp_items==target_value))positions_query=c(positions_query,i)
          if(any(temp_items%in%target_value))positions_query=c(positions_query,i)
          
        }
      }
    }
    
  }
  

  return(positions_query)
  
}


multi_query_fun<-function(user_id="1",session_id="1",target_query,values_query,type_query,data=NULL){
  #target_query="'occupations'=='http://data.europa.eu/esco/isco/C1412'OR'experience_level'=='Mid-level'"
  #user_id="1"
  #session_id="1"
  
  if(is.null(data)){
    data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  }  
  
  target_query_split=unlist(strsplit(target_query,split = ";;"))
  values_query_split=unlist(strsplit(values_query,split = ";;"))
  
  pos_now=c()
  
  for(i in 1:length(target_query_split)){
    if(i==1){
      
      pos_now=query_pos_fun(data,target_query_split[i],values_query_split[i])
      
    }else{
      
      pos_now_temp=query_pos_fun(data,target_query_split[i],values_query_split[i])
      
      if(type_query=="OR"){
        pos_now=union(pos_now,pos_now_temp)
      }else if (type_query=="AND"){
        pos_now = intersect(pos_now, pos_now_temp)      
      }
      
    }
    

  }
  
  pos_now=unique(pos_now)
  
  return(pos_now)
  
}


####Load session
load_user_session_file<-function(user_id="1",session_id="1",attribute="none",subattribute="none"){
  
  
  user_folder=paste0("user_",user_id)
  session_file=paste0("session_",session_id)
  

  if(attribute=="none"){
    
    return(readRDS(paste0(path_user_files_all,user_folder,"/",session_file)))
    
  }else if (attribute=="data_query_info"){
    
    return(readRDS(paste0(path_user_files_all,user_folder,"/",session_file))[['data']][c("count",'endpoint',"query")])
    
  }else{
    if (subattribute=="none"){
      
      return(readRDS(paste0(path_user_files_all,user_folder,"/",session_file))[[attribute]])
      
    }else{
      
      return(readRDS(paste0(path_user_files_all,user_folder,"/",session_file))[[attribute]][[subattribute]])
      
    }
    
  }
  
}


####Save session
save_update_user_session_file<-function(user_id="1",session_id="1",variable_name="query",variable_value="keywords=market",subvariable_name=NULL){
  
  
  #####File variables
  #query
  #data
  #Binary skills
  #Binary occupations
  #Binary propagated skills
  #Binary propagated occupations
  
  all_files=list.files(path_user_files_all)
  
  user_folder=paste0("user_",user_id)
  session_file=paste0("session_",session_id)
  
  
  find_file=user_folder%in%all_files
  
  if(!find_file){
    
    dir.create(paste0(path_user_files_all,user_folder))
    
    data=list()
    
    if(is.null(subvariable_name)){
      data[[variable_name]]=variable_value
      
    }else{
      data[[variable_name]]=list(subvariable_name=variable_value)
    }
    
    
  }else{
    
    user_files=list.files(paste0(path_user_files_all,user_folder,"/"))
    
    find_session=match(session_file,user_files)
    
    if(is.na(find_session)){
      
      data=list()
      
      if(is.null(subvariable_name)){
        data[[variable_name]]=variable_value
        
      }else{
        data[[variable_name]]=list(subvariable_name=variable_value)
      }
      #data[[variable_name]]=variable_value

      
    }else{
      
      data=readRDS(paste0(path_user_files_all,user_folder,"/",session_file))
      
      if(is.null(subvariable_name)){
        
        data[[variable_name]]=variable_value
        
        }else{
          
          if (!(variable_name %in% names(data))){
            data[[variable_name]]=list()
          }
          
          #data[[variable_name]][[subvariable_name]]<-NULL
          #data[[variable_name]]<-c(data[[variable_name]],list(subvariable_name=variable_value))
          data[[variable_name]][[subvariable_name]]<-variable_value
          
        }
       
      
    }
    
  }
  saveRDS(data,paste0(path_user_files_all,user_folder,"/",session_file))
  
}





#* @apiTitle Skillab analytics and skill clustering
#* @apiDescription Conduct descriptive and exploratory analytics on the various data fields. In addition, functionalities for skill clustering and correspondence analysis are supported as well.


#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    forward()
  }
}


#* Load Data from API
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param url Url of the endpoint (e.g. https://skillab-tracker.csd.auth.gr/api/jobs)
#* @param body Body for the POST API call (e.g. keywords=seller&keywords=market)
#* @param limit_data_no Maximum number of observations to return. Leave empty if you want to return all.
#* @get /load_data
function(url,body,user_id,session_id,limit_data_no=""){
  
  print("Loading Data")
  
  future({
    
    data_now<-api_ex_now(url, body,limit_data_no = limit_data_no)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'data',variable_value = data_now)
    return(data_now)
  }) %...>% {  # On success
    
    data <- . 
    
    
    return(paste("Data loaded successfully. The number of records is", data$count))
  
    } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
  
}

#* Retrieve information and outputs from a session 
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param attribute The attribute to return. Current options: none - Returns everything , data - Returns all data, data_query_info - Returns the query information, all_stats - Returns output from Descriptive statistics,  all_stats_prop - Returns output from Descriptive statistics with propagation, explor_stats - Returns output from Exploratory Analytics, explor_stats_prop - Returns output from Exploratory Analytics with propagation, explor_stats_one - Returns output from Exploratory analysis given a filter, explor_stats_one_prop - Returns output from Exploratory analysis given a filter with propagation,  trend_anal - Returns output from Trend Analysis, trend_anal_prop - Returns output from Trend Analysis with propagation, super_freq - Return the evaluations from the frequencies_super_fun, skill_clust - Returns output from  Cluster analysis, multiple_cor - Returns output from Multiple Correspondence analysis
#* @param storage_name Code name of the desired outcome. Leave this as none if you want to retrieve every item within the attribute. This utility enables the existence of multiple outputs for each type of analysis.
#* @get /get_data
function(user_id,session_id,attribute="none",storage_name="none"){
  future({
    return(load_user_session_file(user_id = user_id,session_id = session_id,attribute = attribute,subattribute = storage_name))
    
  }) %...>%{  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
}

analytics_fun<-function(user_id="1",session_id="1",features_query="",data=NULL){
  
  if(is.null(data)){
    data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  }
  

  features_query_split=unlist(strsplit(features_query,";;"))
  
  data_now_list=list()
  
  for (f in features_query_split){
    
      data_now_list[[f]]= unlist(
        lapply(data$items, function(x) {
          if (length(x[[f]]) > 0 & !is.null(x[[f]])) {
            
            return(unique(unlist(x[[f]])))
            
          }
        })
      )
      
      
      data_now_list[[f]]=table(data_now_list[[f]])
      
      if(length(data_now_list[[f]])==1){
        
        data_now_list[[f]]=data.frame(Item=names(data_now_list[[f]]),Freq=as.numeric(data_now_list[[f]]))
        
      }else{
        
        data_now_list[[f]]=as.data.frame(sort(data_now_list[[f]],decreasing = T))
        
      }
      
      
      if (f=="skills"){
        
        
        #data_now_list[[f]]$label
        data_now_list[[f]]$label=unlist(lapply(data_now_list[[f]][,1],function(x){
          
          x=as.character(x)
          match_ids_now=match(x,data_all_skills$id)
          if(is.na(match_ids_now)){
            match_ids_now=x
          }else{
            match_ids_now=data_all_skills$label[match_ids_now]
          }          
          
          return(match_ids_now)
          
        }))
        
        
      }else if (f=="occupations"){
        
        #data_now_list[[f]]$label
        data_now_list[[f]]$label=unlist(lapply(data_now_list[[f]][,1],function(x){
          
          x=as.character(x)
          match_ids_now=match(x,data_all_occupations$id)
          if(is.na(match_ids_now)){
            match_ids_now=x
          }else{
            match_ids_now=data_all_occupations$label[match_ids_now]
          }
          return(match_ids_now)
          
        }))
        
        
      }
      
    
    
    
  }
  
  gc()
  
  return(data_now_list)
  
}

#* Descriptive statistics
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param features_query Which features_query to be extracted from the imported data. They should be separated using the string ';;' (e.g. skills;;occupations;;location;;type)
#* @get /analytics_descriptive
function(user_id,session_id,storage_name,features_query){
 
  print("Descriptive statistics")
  
  future({
    
    data_now=analytics_fun(user_id = user_id, session_id = session_id,features_query = features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'all_stats',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
}



analytics_prop_fun<-function(user_id="1",session_id="1",what='skills',pillar="",level="",data=NULL){
  
  #data_now=api_ex_now(url,body)
  if(is.null(data))data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  
  binary_mat_double_to_occur=return_double_to_occur_propagate(data,what = what,pillar = pillar,level=level)
  
  
    
    col_sums=colSums(binary_mat_double_to_occur)
    col_sums=data.frame(names(col_sums),as.numeric(col_sums))
    
  

  if(what=="occupations"){
    col_sums$label=unlist(data_all_occupations$label[match(col_sums[,1],data_all_occupations$id)])
    
  }else if(what=="skills"){
    col_sums$label=unlist(data_all_skills$label[match(col_sums[,1],data_all_skills$id)])
    
  }
  
  col_sums=col_sums[order(col_sums[,2],decreasing = T),]
  colnames(col_sums)=c("Id","Frequency","Label")
  
  return(col_sums)
  
  
}

#* Descriptive statistics with propagation for skill/occupation pillar and level
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param what What to investigate. This has to be either skills or occupations
#* @param pillar Pillars to analyze, only used for skills. It can contain multiple pillars separated by ';;'. Available options: Skill, Knowledge, Traversal, Language.
#* @param level Taxonomy Levels to investigate. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ';;' (e.g. 0;;3;;2).
#* @get /analytics_descriptive_propagation
function(user_id,session_id,storage_name,what,pillar="",level=""){
  
  print("Descriptive statistics with propagation")
  
  future({
    
    data_now=analytics_prop_fun(user_id = user_id, session_id = session_id,what=what,pillar=pillar,level=level)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'all_stats_prop',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
}


exploratory_fun<-function(user_id="1",session_id="1",features_query="",data=NULL){
  
  if(is.null(data))data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  
  #data_now=api_ex_now(url,body)
  features_query_split=unlist(strsplit(features_query,";;"))
  
  if(features_query_split[1]==features_query_split[2]){

    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        x_list=unlist(x[[features_query_split[1]]])  
        x_list=unique(x_list)
        
        if(length(x_list)>1&!is.null(x_list)){
          
          return_list=list(c1=c(),c2=c())
          

          x_list=sort(x_list)
          
          for(i in 1:(length(x_list)-1)){
            for(j in (i+1):length(x_list)){
              
              return_list$c1=c(return_list$c1,x_list[i])
              return_list$c2=c(return_list$c2,x_list[j])
              
            }
          }
          return(as.data.frame(return_list))
          
        }
        
        
      })
    ) 
      
  }else{
    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        x1_list=unlist(x[[features_query_split[1]]])  
        x2_list=unlist(x[[features_query_split[2]]])
        
        if(length(x1_list)>0&!is.null(x1_list)&length(x2_list)>0&!is.null(x2_list)){
          x1_list=unique(x1_list)
          x2_list=unique(x2_list)
          
          return(expand.grid(x1_list,x2_list))
          
        }
        
        
      })
    )
    
    
  }

  data_now=table(data_now) ; data_now=as.data.frame(data_now)
  
  

  data_now=data_now[data_now$Freq>0,]
  data_now=data_now[order(data_now$Freq,decreasing = T),]
  
  
  for (i in 1:length(features_query_split)){
    if(features_query_split[i]=="occupations"){
      data_now[[paste0("item_",i)]]=unlist(data_all_occupations$label[match(data_now[,i],data_all_occupations$id)])
      
    }else if(features_query_split[i]=="skills"){
      data_now[[paste0("item_",i)]]=unlist(data_all_skills$label[match(data_now[,i],data_all_skills$id)])
      
    }
    
  }
  
  
  return(data_now)
  
}

#* Exploratory Analytics
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param features_query Which features_query to be extracted from the imported data. They should be exactly two features separated using the character ';;' (e.g. skills;;occupations;;location,type)
#* @get /analytics_exploratory
function(user_id,session_id,storage_name,features_query){
  
  print("Exploratory statistics")
  
  future({
    
    data_now=exploratory_fun(user_id = user_id, session_id = session_id,features_query = features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'explor_stats',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
 
  
}



exploratory_fun_prop<-function(user_id="1",session_id="1",features_query="",pillar_1="",pillar_2="",level_1="",level_2="",data=NULL){
  

  if(is.null(data)) data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  features_query=unlist(strsplit(features_query,";;"))
  
  data_now_list=list()
  
  for (i in 1:2){
    if(i==1){
      
      pillar_now=pillar_1
      level_now=level_1
      
    }else if (i==2){
      
      pillar_now=pillar_2
      level_now=level_2
      
    }
    
    
    if(nchar(level_now)!=0){
      
      data_now_list[[features_query[i]]]=return_double_to_occur_propagate(data = data,what = features_query[i],pillar = pillar_now,level=level_now)
      
      
    }else{
      
        data_now_temp <- do.call(
          rbind,
          lapply(data$items, function(x) {
            if (length(x[[features_query[i]]]) > 0 & !is.null(x[[features_query[i]]])) {
              return(data.frame(item_id = x$id, item = unique(unlist(x[[features_query[i]]])), stringsAsFactors = FALSE))
            }
          })
        )
        
      data_now_list[[features_query[i]]]=double_to_occur(data_now_temp)
      
    }
    
    
    
  }
  
  
which_excl_1=which(!(rownames(data_now_list[[features_query[1]]])%in%rownames(data_now_list[[features_query[2]]])))
if(length(which_excl_1)>0)data_now_list[[features_query[1]]]=data_now_list[[features_query[1]]][-which_excl_1,]

which_excl_2=which(!(rownames(data_now_list[[features_query[2]]])%in%rownames(data_now_list[[features_query[1]]])))
if(length(which_excl_2)>0)data_now_list[[features_query[2]]]=data_now_list[[features_query[2]]][-which_excl_2,]

#check_row_names=table(rownames(data_now_list[[features_query[1]]])==rownames(data_now_list[[features_query[2]]]))

co_occurence_mat_now=co_occurence_mat(data_now_list[[features_query[1]]],data_now_list[[features_query[2]]])
#co_occurence_mat_now=data.frame("Item_1"=rownames(co_occurence_mat_now)[as.vector(row(co_occurence_mat_now))],"Item_2"=colnames(co_occurence_mat_now)[as.vector(col(co_occurence_mat_now))],"Freq"=as.vector(co_occurence_mat_now))
co_occurence_mat_now=as.data.frame(as.table(co_occurence_mat_now))

which_0=which(co_occurence_mat_now$Freq ==0 )
if(length(which_0)>0)  co_occurence_mat_now=co_occurence_mat_now[-which_0,]
co_occurence_mat_now=co_occurence_mat_now[order(co_occurence_mat_now$Freq,decreasing = T),]


for (i in 1:length(features_query)){
  if(features_query[i]=="occupations"){
    co_occurence_mat_now[[paste0("item_",i)]]=unlist(data_all_occupations$label[match(co_occurence_mat_now[,i],data_all_occupations$id)])
    
  }else if(features_query[i]=="skills"){
    co_occurence_mat_now[[paste0("item_",i)]]=unlist(data_all_skills$label[match(co_occurence_mat_now[,i],data_all_skills$id)])
    
  }
  
}





return(co_occurence_mat_now)

}

#* Exploratory Analytics with propagation
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param features_query Which features_query to be extracted from the imported data. They should be exactly two features separated using the character ',' (e.g. skills, occupations, location,type)
#* @param pillar_1 Pillars to analyze for the first variable, only used for skills. It can contain multiple pillars separated by ','. Available options: Skill, Knowledge, Traversal, Language. It should be empty if propagation is not required.
#* @param level_1 Taxonomy Levels to investigate for the first variable. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ',' (e.g. 0,3,2). It should be empty if propagation is not required
#* @param pillar_2 Pillars to analyze for the second variable, only used for skills. It can contain multiple pillars separated by ','. Available options: Skill, Knowledge, Traversal, Language. It should be empty if propagation is not required.
#* @param level_2 Taxonomy Levels to investigate for the second variable. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ',' (e.g. 0,3,2). It should be empty if propagation is not required
#* @get /analytics_exploratory_prop
function(user_id,session_id,storage_name,features_query,pillar_1="",pillar_2="",level_1="",level_2=""){
  
  print("Exploratory statistics with propagation")
  
  future({
    
    data_now=exploratory_fun_prop(user_id = user_id, session_id = session_id,features_query = features_query,pillar_1=pillar_1,pillar_2=pillar_2,level_1=level_1,level_2=level_2)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'explor_stats_prop',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
}


exploratory_fun_one<-function(user_id="1",session_id="1",target_feature,target_values,features_query,data=NULL){
  
  if(is.null(data)) data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  data<-data$items
  
  #data_now=api_ex_now(url,body)
  features_query_split=unlist(strsplit(features_query,";;"))
  target_values_split=unlist(strsplit(target_values,";;"))
  
  counter_now=0
  
  data_now_list=list()
  
  for (f in features_query_split){
    data_now_list[[f]]=list()
  }
  
  
  for (i in 1:length(data)){
    
    if(length(data[[i]][[target_feature]]) > 0 & !is.null(data[[i]][[target_feature]])){
      
      tf=unlist(lapply(data[[i]][[target_feature]], function(y) {
        y
      }))
      
      if (any(tf%in%target_values_split)){
        
        counter_now=counter_now+1
        
        for (f in features_query_split){
          
         if(length(data[[i]][[f]]) > 0 & !is.null(data[[i]][[f]])){
           
           ff=unique(unlist(lapply(data[[i]][[f]], function(y) {
             y
           })))
           
           data_now_list[[f]]=c(data_now_list[[f]],ff)
           
         } 
          
        }
        
      }
      
    }
  }
  

  for (f in features_query_split){
      
      data_now_list[[f]]=table(unlist(data_now_list[[f]]))
      
      if(length(data_now_list[[f]])==1){
        
        data_now_list[[f]]=data.frame(Item=names(data_now_list[[f]]),Freq=as.numeric(data_now_list[[f]]))
        
      }else{
        
        data_now_list[[f]]=as.data.frame(data_now_list[[f]])
        data_now_list[[f]]=data_now_list[[f]][order( data_now_list[[f]]$Freq,decreasing = T),]

      }

    } 
    
  data_now_list$counter=counter_now
  
  return(data_now_list)
  
}

#* Exploratory Analytics with filtering
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param target_feature Data feature to inspect (e.g. skills, occupations, location,type).
#* @param target_values Values of the target feature to inspect. Multiple values are supported and can be separated with the coma (";;").
#* @param features_query Which features_query to be extracted from the imported data. They should be separated using the string ';;' (e.g. skills;;occupations;;location;;type)
#* @get /analytics_exploratory_filt
function(user_id,session_id,storage_name,target_feature,target_values,features_query){
  
  print("Exploratory statistics with filtering")
  
  future({
    
    data_now=exploratory_fun_one(user_id = user_id, session_id = session_id,target_feature = target_feature,target_values = target_values,features_query = features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'explor_stats_one',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
}



exploratory_fun_one_prop<-function(user_id="1",session_id="1",storage_name="",target_feature,target_values,features_query="",pillar="",level="",data=NULL){
  
  

  if(is.null(data)) data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  data<-data$items
  
  target_values_split=unlist(strsplit(target_values,";;"))

  data_now_list_target=list()
  
  
  if(target_feature=="skills_prop"){
    tf_now_temp="skills"
  }else if(target_feature=="occupations_prop"){
    tf_now_temp="occupations"
  }else{
    tf_now_temp=target_feature
    
  }
  
  
  for (i in 1:length(data)){
    
    if (length(data[[i]][[tf_now_temp]]) > 0 & !is.null(data[[i]][[tf_now_temp]]) ) {
      tf_list=unlist((data[[i]][[tf_now_temp]]))
      for (tf_temp in tf_list){
        if(is.null(data_now_list_target[[tf_temp]])){
          data_now_list_target[[tf_temp]]=c(i)
        }else{
          data_now_list_target[[tf_temp]]=c(data_now_list_target[[tf_temp]],i)
        }
      }
    }
    
  }
  
  names_target=names(data_now_list_target)
  names_target_in=c()
  
  if(target_feature=="skills_prop"){
    
    for (nt in names_target){
      i=match(nt,data_all_skills$id)
      temp_ancestors=unlist(data_all_skills[i,c('id',"skill_ancestors","knowledge_ancestors","traversal_ancestors","language_ancestors")])
      
      if(any(target_values_split%in%temp_ancestors)){
        names_target_in=c(names_target_in,nt)
      }
      
    }
    
  }else if(target_feature=="occupations_prop"){
    
    for (nt in names_target){
      i=match(nt,data_all_occupations$id)
      
      temp_ancestors=unlist(data_all_occupations[i,c('id',"ancestors")])
      
      if(any(target_values_split%in%temp_ancestors)){
        names_target_in=c(names_target_in,nt)
      }
      
    }
    
  }else{
    
    for (nt in names_target){
      

      if(any(target_values_split%in%c(nt))){
        names_target_in=c(names_target_in,nt)
      }
      
    }
    
  }
  
  data_now_list_target=data_now_list_target[names_target_in]
  
  data_now_list_target=unique(unlist(data_now_list_target))
  
  
  data_now_list_feature=list()
  
  
  for (i in data_now_list_target){
    
    if (length(data[[i]][[features_query]]) > 0 & !is.null(data[[i]][[features_query]]) ) {
      ff_list=unlist(data[[i]][[features_query]])
      
      for (ff_temp in ff_list){
        if(is.null(data_now_list_feature[[ff_temp]])){
          data_now_list_feature[[ff_temp]]=c(i)
        }else{
          data_now_list_feature[[ff_temp]]=c(data_now_list_feature[[ff_temp]],i)
        }
      }
      
    }
    
  }

  
  
  if(features_query=="skills"){
    
    if(nchar(level)>0){
      #Skill, Knowledge, Traversal, Language
      #skill_ancestors , knowledge_ancestors , traversal_ancestors , language_ancestors

    
      table_map=propagation_skills(data = NULL,urls=names(data_now_list_feature),pillar = pillar,level = level)
      
      
      data_now_list_feature_new=list()
      
      for (name_now_org in names(data_now_list_feature)){
        
        which_in=which(table_map[,1]==name_now_org)
        if(length(which_in)>0){
          for (name_now_anc in table_map[which_in,2]){
            data_now_list_feature_new[[name_now_anc]]=c(data_now_list_feature_new[[name_now_anc]],unlist(data_now_list_feature[[name_now_org]]))
          }
        }
      }
    
      data_now_list_feature=data_now_list_feature_new
      
    }
    
    
  }else if(features_query=="occupations"){
    
    if(nchar(level)>0){
      
      table_map=propagation_occupations(data = NULL,urls=names(data_now_list_feature),level = level)
      
      
      data_now_list_feature_new=list()
      
      for (name_now_org in names(data_now_list_feature)){
        
        which_in=which(table_map[,1]==name_now_org)
        if(length(which_in)>0){
          for (name_now_anc in table_map[which_in,2]){
            data_now_list_feature_new[[name_now_anc]]=c(data_now_list_feature_new[[name_now_anc]],unlist(data_now_list_feature[[name_now_org]]))
          }
        }
        
      
      }
      
      data_now_list_feature=data_now_list_feature_new
      
    }
    
  }
  
  for(i in 1:length(data_now_list_feature))data_now_list_feature[[i]]=unique(data_now_list_feature[[i]])
  
  data_now_list_feature=data.frame("Item"=names(data_now_list_feature),"Freq"=unlist(lapply(data_now_list_feature,function(x)length(x))))
  data_now_list_feature=data_now_list_feature[order(data_now_list_feature$Freq,decreasing = T),]

  
  
  if(features_query=="occupations"){
      data_now_list_feature[[features_query]]=unlist(data_all_occupations$label[match(data_now_list_feature[,1],data_all_occupations$id)])
      
  }else if(features_query=="skills"){
      data_now_list_feature[[features_query]]=unlist(data_all_skills$label[match(data_now_list_feature[,1],data_all_skills$id)])
      
  }
    
  
  return(data_now_list_feature)
    
}



#* Exploratory Analytics with filtering and propagation
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param target_feature Data feature to inspect (e.g. skills, occupations, location,type). If target_feature is equal to 'skills_prop' or 'occupations_prop' then the data are filtered with propagation
#* @param target_values Values of the target feature to inspect. Multiple values are supported and can be separated with the coma (";;"). If target_feature is equal to 'skills_prop' or 'occupations_prop' then these target values are searched in the ESCO hierarchy trees
#* @param features_query Which feature to be inspected for exploratory analysis with respect to the target values. It should be exactly one feature e.g. skills, occupations, location,type).
#* @param pillar Pillars to analyze, only used for skills. It can contain multiple pillars separated by ';;'. Available options: Skill, Knowledge, Traversal, Language. It should be empty if propagation is not required.
#* @param level Taxonomy Levels to investigate. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ';;' (e.g. 0,3,2). It should be empty if propagation is not required.
#* @get /analytics_exploratory_filt_prop
function(user_id,session_id,storage_name,target_feature,target_values,features_query,pillar="",level=""){
  
  print("Exploratory statistics with filtering and propagation")
  print(pillar)
  print(level)
    
  future({
    

    
    data_now=exploratory_fun_one_prop(user_id = user_id, session_id = session_id,target_feature = target_feature,target_values = target_values,features_query = features_query,pillar=pillar,level=level)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'explor_stats_one_prop',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
  
  
}


trend_anal_fun<-function(user_id="1",session_id="1",date_field="upload_date",features_query="",date_format="%Y-%m-%d",what="year",data=NULL){
  
  if(is.null(data)) data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  features_query_split=unlist(strsplit(features_query,";;"))
  
  if(what=="year"){
    digits_to_see_start=1
    digits_to_see_end=4
  }else if (what=="month"){
    digits_to_see_start=6
    digits_to_see_end=7
  }else if (what=="year_month"){
    digits_to_see_start=1
    digits_to_see_end=7
  }
  
  dates_now=lapply(data$items,function(x){
    temp_now=unlist(strsplit(x[[date_field]]," "))[[1]]
    #temp_now=as.Date(x = temp_now,format=date_format)
    temp_now=substr(temp_now,start=digits_to_see_start,stop = digits_to_see_end)
    return(temp_now)
  })
    
  dates_now=unlist(as.matrix(dates_now,ncol=1))
  #rownames(dates_now)=NULL
  
  gc()
  
  data_now=list()

  for (f in features_query_split){
    
    
      
      data_now[[f]] <- do.call(
        rbind,
        lapply(c(1:length(dates_now)), function(i) {
          
          if (length(data$items[[i]][[f]]) > 0 & !is.null(data$items[[i]][[f]]) & !is.null(dates_now[i])) {

                  
          return(data.frame(date = dates_now[i], item = unique(unlist(data$items[[i]][[f]])), stringsAsFactors = FALSE))
                  
             
              
            
          }
        })
      )
      

    data_now[[f]]=as.data.frame(table(data_now[[f]]))
    
    
    which_0=which(data_now[[f]]$Freq==0)
    if(length(which_0)>0) data_now[[f]]=data_now[[f]][-which_0,]
    data_now[[f]]=data_now[[f]][order(data_now[[f]]$Freq,decreasing = T),]
    
    
    if(f=="occupations"){
      data_now[[f]]$label=unlist(data_all_occupations$label[match(data_now[[f]][,2],data_all_occupations$id)])
      
    }else if(f=="skills"){
      data_now[[f]]$label=unlist(data_all_skills$label[match(data_now[[f]][,2],data_all_skills$id)])
      
    }
    
  }
  
  return(data_now)
  
}


#* Trend analysis with propagation
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param date_field Variable storing the date within the data
#* @param features_query Which features_query to be extracted from the imported data. They should be separated using the character ';;' (e.g. skills;;occupations;;location;;type)
#* @param date_format The date format of the stored data. Example (2025-01-02): "%Y-%m-%d"
#* @param what What to investigate regarding dates-trends. Possible values: 'year' , 'month' , 'year_month'.
#* @get /trend_analysis
function(user_id,session_id,storage_name,date_field="upload_date",features_query,date_format="%Y-%m-%d",what="year"){
  print("Trend Analysis")
  
  
  future({
    
    data_now=trend_anal_fun(user_id=user_id,session_id=session_id,date_field=date_field,features_query=features_query,date_format=date_format,what=what)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'trend_anal',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
}


trend_anal_fun_prop<-function(user_id="1",session_id="1",date_field="upload_date",features_query="",date_format="%Y-%m-%d",what="year",pillar,level,data=NULL){

  if(is.null(data)) data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  

  if(what=="year"){
    digits_to_see_start=1
    digits_to_see_end=4
  }else if (what=="month"){
    digits_to_see_start=6
    digits_to_see_end=7
  }else if (what=="year_month"){
    digits_to_see_start=1
    digits_to_see_end=7
  }
  
  dates_now=do.call(
    rbind,
    lapply(data$items,function(x){
    temp_now=unlist(strsplit(x[[date_field]]," "))[[1]]
    #temp_now=as.Date(x = temp_now,format=date_format)
    temp_now=substr(temp_now,start=digits_to_see_start,stop = digits_to_see_end)
    return(data.frame("Id"=x$id,"Date"=temp_now))
  })
  )
  
  dates_now=double_to_occur(dates_now)
  
  gc()
  
  
  data_now=return_double_to_occur_propagate(data = data, what = features_query,pillar = pillar,level = level)
  
  
  which_excl_1=which(!(rownames(dates_now)%in%rownames(data_now)))
  if(length(which_excl_1)>0)dates_now=dates_now[-which_excl_1,]
  
  which_excl_2=which(!(rownames(data_now)%in%rownames(dates_now)))
  if(length(which_excl_2)>0)data_now=data_now[-which_excl_2,]
  

  co_occurence_mat_now=co_occurence_mat(data_now,dates_now)
  #co_occurence_mat_now=data.frame("Item_1"=rownames(co_occurence_mat_now)[as.vector(row(co_occurence_mat_now))],"Item_2"=colnames(co_occurence_mat_now)[as.vector(col(co_occurence_mat_now))],"Freq"=as.vector(co_occurence_mat_now))
  co_occurence_mat_now=as.data.frame(as.table(co_occurence_mat_now))
  
  which_0=which(co_occurence_mat_now$Freq ==0 )
  if(length(which_0)>0)  co_occurence_mat_now=co_occurence_mat_now[-which_0,]
  co_occurence_mat_now=co_occurence_mat_now[order(co_occurence_mat_now$Freq,decreasing = T),]
  
  if(features_query=="occupations"){
    co_occurence_mat_now$label=unlist(data_all_occupations$label[match(co_occurence_mat_now[,1],data_all_occupations$id)])
    
  }else if(features_query=="skills"){
    co_occurence_mat_now$label=unlist(data_all_skills$label[match(co_occurence_mat_now[,1],data_all_skills$id)])
    
  }
  
  return(co_occurence_mat_now)
  
}

#* Trend analysis with propagation
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param date_field Variable storing the date within the data
#* @param features_query Which feature to be extracted from the imported data. It should be exactly one (skills or occupations)
#* @param date_format The date format of the stored data. Example (2025-01-02): "%Y-%m-%d"
#* @param what What to investigate regarding dates-trends. Possible values: 'year' , 'month' , 'year_month'.
#* @param pillar Pillars to analyze, only used for skills. It can contain multiple pillars separated by ';;'. Available options: Skill, Knowledge, Traversal, Language.
#* @param level Taxonomy Levels to investigate. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ';;' (e.g. 0,3,2).
#* @get /trend_analysis_prop
function(user_id,session_id,storage_name,date_field="upload_date",features_query,date_format="%Y-%m-%d",what="year",pillar,level){
  print("Trend Analysis with propagation")
  
  
  future({
    
    data_now=trend_anal_fun_prop(user_id=user_id,session_id=session_id,date_field=date_field,features_query=features_query,date_format=date_format,what=what,pillar = pillar,level = level)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'trend_anal_prop',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
}


unanticipated_freq_fun=function(freq_in,freq_all,no_documents_query,no_documents_all){
  
  final_score=c()
  for(i in 1:length(freq_in)){
    prop_in=freq_in/no_documents_query
    prop_out=(freq_all-freq_in+.Machine$double.eps)/(no_documents_all-no_documents_query+.Machine$double.eps)
    final_score[i]=prop_in/prop_out
  }
  
  
  return(final_score)
  
}


unanticipated_freq_multi_fun_one<-function(data_query_temp,data_all_temp,counter_now,counter_all){
  
  unant_scores=list()
  
  for(f in names(data_query_temp)){
    
    if(nrow(data_query_temp[[f]])>1){
      
      unant_scores[[f]]=list("Item"=c(),"Freq_in"=c(),"Freq_all"=c(),"Score"=c())
      
      match_in=match(data_query_temp[[f]][,1],data_all_temp[[f]][,1])
      match_in_na=which(is.na(match_in))
      match_not_na=which(!is.na(match_in))
      
      
      if(length(match_in_na)>0){
        #match_in=match_in[-match_in_na]
        unant_scores[[f]]$Item=c(unant_scores[[f]]$Item,as.character(data_query_temp[[f]][match_in_na,1]))
        unant_scores[[f]]$Freq_in=c(unant_scores[[f]]$Freq_in,data_query_temp[[f]][match_in_na,2])
        unant_scores[[f]]$Freq_all=c(unant_scores[[f]]$Freq_all,rep(0,length(match_in_na)))
        unant_scores[[f]]$Score=c(unant_scores[[f]]$Score,rep(Inf,length(match_in_na)))
        
      }
      
      if(length(match_not_na)!=0){
        unant_scores[[f]]$Item=c(unant_scores[[f]]$Item,as.character(data_query_temp[[f]][match_not_na,1]))
        unant_scores[[f]]$Freq_in=c(unant_scores[[f]]$Freq_in,data_query_temp[[f]][match_not_na,2])
        unant_scores[[f]]$Freq_all=c(unant_scores[[f]]$Freq_all,data_all_temp[[f]][match_in[match_not_na],2])
        for (i in 1:length(match_not_na)){
          unant_scores[[f]]$Score=c(unant_scores[[f]]$Score,unanticipated_freq_fun(
            freq_in = data_query_temp[[f]][match_not_na[i],2],
            freq_all = data_all_temp[[f]][match_in[match_not_na][i],2],
            no_documents_query = counter_now,
            no_documents_all = counter_all
          ))
          
        }
        
      }
      
      match_all_na=match(data_all_temp[[f]][,1],data_query_temp[[f]][,1])
      match_all_na=which(is.na(match_all_na))
      if(length(match_all_na)>0){
        unant_scores[[f]]$Item=c(unant_scores[[f]]$Item,as.character(data_all_temp[[f]][match_all_na,1]))
        unant_scores[[f]]$Freq_in=c(unant_scores[[f]]$Freq_in,rep(0,length(match_all_na)))
        unant_scores[[f]]$Freq_all=c(unant_scores[[f]]$Freq_all,data_all_temp[[f]][match_all_na,2])
        unant_scores[[f]]$Score=c(unant_scores[[f]]$Score,rep(0,length(match_all_na)))
        
      }
      unant_scores[[f]]=as.data.frame(unant_scores[[f]])
      if(f=="skills"){
        unant_scores[[f]]$Label=data_all_skills$Label[match(unant_scores[[f]]$Item,data_all_skills$id)]
      }else if (f=="occupations"){
        unant_scores[[f]]$Label=data_all_occupations$Label[match(unant_scores[[f]]$Item,data_all_occupations$id)]
        
      }
    }
  }
  
  unant_scores$count_query=counter_now
  unant_scores$count_all=counter_all
  
  return(unant_scores)
}


unanticipated_freq_multi_fun_double<-function(data_query_temp,data_all_temp,counter_now,counter_all){
  
  #data_query_temp=data_query_res
  #data_all_temp=data_all_res
  
  unant_scores_list=list()
  
  for (f in names(data_query_temp)){
    
    unant_scores=list()
    unant_scores[["table"]]=list("Item_1"=c(),"Item_2"=c(),"Freq_in"=c(),"Freq_all"=c(),"Score"=c())  
    
    data_query_temp_now=data_query_temp[[f]]
    data_all_temp_now=data_all_temp[[f]]
    
    data_query_temp_now[,1]=as.character(data_query_temp_now[,1]);data_query_temp_now[,2]=as.character(data_query_temp_now[,2])
    data_all_temp_now[,1]=as.character(data_all_temp_now[,1]);data_all_temp_now[,2]=as.character(data_all_temp_now[,2])
    
    string_query=c()
    
    
    for(i in 1:nrow(data_query_temp_now))string_query=c(string_query,paste(data_query_temp_now[i,1],data_query_temp_now[i,2],";"))
      
    
    
    for(i in 1:nrow(data_all_temp_now)){
      
      string_temp=paste(data_all_temp_now[i,1],data_all_temp_now[i,2],";")
      match_temp=match(string_temp,string_query)
      
      if(is.na(match_temp)){
        unant_scores[["table"]]$`Item_1`=c( unant_scores[["table"]]$`Item_1`,data_all_temp_now[i,1])
        unant_scores[["table"]]$`Item_2`=c(unant_scores[["table"]]$`Item_2`,data_all_temp_now[i,2])
        unant_scores[["table"]]$Freq_in=c(unant_scores[["table"]]$Freq_in,0)
        unant_scores[["table"]]$Freq_all=c(unant_scores[["table"]]$Freq_all,data_all_temp_now[i,3])
        unant_scores[["table"]]$Score=c(unant_scores[["table"]]$Score,0)
      }else{
        unant_scores[["table"]]$`Item_1`=c( unant_scores[["table"]]$`Item_1`,data_all_temp_now[i,1])
        unant_scores[["table"]]$`Item_2`=c(unant_scores[["table"]]$`Item_2`,data_all_temp_now[i,2])
        unant_scores[["table"]]$Freq_in=c(unant_scores[["table"]]$Freq_in,data_query_temp_now[match_temp,3])
        unant_scores[["table"]]$Freq_all=c(unant_scores[["table"]]$Freq_all,data_all_temp_now[i,3])
        unant_scores[["table"]]$Score=c(unant_scores[["table"]]$Score,
                                        unanticipated_freq_fun(freq_in = data_query_temp_now[match_temp,3],freq_all = data_all_temp_now[i,3],no_documents_query = counter_now,no_documents_all = counter_all)
        )
      }
      
    }
    
    unant_scores_list[[f]]=as.data.frame(unant_scores$table)
    
  }
  
  
  
  
  unant_scores_list$count_query=counter_now
  unant_scores_list$count_all=counter_all
  
  return(unant_scores_list)
  
}


frequencies_filtered_call_superfun<-function(user_id="",session_id="",target_feature="",target_values="",features_query,type_analysis,pillar_1="",pillar_2="",level_1="",level_2="",unanticipated_freq_arg,type_query,date_field,data=NULL){
  
  #target_values='http://data.europa.eu/esco/isco/C1412'
  #target_feature='occupations'
  
  #target_feature="experience_level"
  #target_values="Mid-level"
  
  #target_feature="skill_propagation"
  #target_value="http://data.europa.eu/esco/isced-f/06"
  
  #target_feature="occupation_propagation"
  #target_values="http://data.europa.eu/esco/isco/C12"  
  
  if(is.null(data)) data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  #data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  
  #target_values_split=unlist(strsplit(target_values,";;"))
  
  counter_all=data$count
  counter_now=0
  
  if(nchar(target_feature)>0){
    pos_now=multi_query_fun(user_id = user_id,session_id = session_id,target_query = target_feature,values_query = target_values,type_query = type_query,data = data)
  }else{
    pos_now=c(1:length(data$items))
  }
  counter_now=length(pos_now)
  
  
  #
  #for(i in 1:length(data$items)){
   # temp_fields=unlist(data$items[[i]][[target_feature]])
    #if(length(temp_fields)>0){
     # if(any(temp_fields%in%target_values)){
      #  counter_now=counter_now+1
      #}
    #}
  #}
  
  if(counter_now>0){
    
    data_temp=data
    data_temp$items=data_temp$items[pos_now]
    
    if(type_analysis=="ONE"){
      #features_query="skills"
      #pillar_1="Skill,Knowledge";level_1="1,2"
      
      if(nchar(level_1)>0){
        data_query_res=list()
        data_query_res[[features_query]]=analytics_prop_fun(user_id = user_id,session_id=session_id,what = features_query,pillar = pillar_1,level = level_1,data = data_temp)
        
        
        if(unanticipated_freq_arg=="YES"){
          data_all_res=list()
          data_all_res[[features_query]]=analytics_prop_fun(user_id = user_id,session_id=session_id,what = features_query,pillar = pillar_1,level = level_1,data = data)
          unant_scores=unanticipated_freq_multi_fun_one(data_query_res,data_all_res,counter_now,counter_all)
          
        }else{
          unant_scores=data_query_res
          unant_scores$count_query=counter_now
          unant_scores$count_all=counter_all
        }
        
      }else{
        
        data_query_res=analytics_fun(user_id = user_id,session_id=session_id,features_query = features_query,data = data_temp)
        if(unanticipated_freq_arg=="YES"){
          data_all_res=analytics_fun(user_id = user_id,session_id=session_id,features_query = features_query,data = data)
          unant_scores=unanticipated_freq_multi_fun_one(data_query_res,data_all_res,counter_now,counter_all)
          
        }else{
          unant_scores=data_query_res
          unant_scores$count_query=counter_now
          unant_scores$count_all=counter_all
        }
        
      }
     

      
      
    }else if (type_analysis=="PAIR"){
      
      #features_query="skills,occupations"
      #pillar_1="Skill,Knowledge";pillar_2="";level_1="1,2";level_2=""
      data_query_res=list()
      
      if(level_1==""&level_2==""){
        data_query_res[[features_query]]=exploratory_fun(user_id = user_id,session_id=session_id,features_query = features_query,data = data_temp)
        
      }else{
        data_query_res[[features_query]]=exploratory_fun_prop(user_id = user_id,session_id=session_id,features_query = features_query,pillar_1 = pillar_1,pillar_2 = pillar_2,level_1 = level_1,level_2 = level_2,data = data_temp)
        
      }
      
      if(unanticipated_freq_arg=="YES"){
        data_all_res=list()
        
        if(level_1==""&level_2==""){
          data_all_res[[features_query]]=exploratory_fun(user_id = user_id,session_id=session_id,features_query = features_query,data = data)
          
        }else{
          data_all_res[[features_query]]=exploratory_fun_prop(user_id = user_id,session_id=session_id,features_query = features_query,pillar_1 = pillar_1,pillar_2 = pillar_2,level_1 = level_1,level_2 = level_2,data = data)
          
        }
        
        unant_scores=unanticipated_freq_multi_fun_double(data_query_res,data_all_res,counter_now,counter_all)
        
      }else{
        unant_scores=list()
        unant_scores[[features_query]]=data_query_res
        unant_scores$count_query=counter_now
        unant_scores$count_all=counter_all
      }

      
    }else if(substr(type_analysis,1,5)=="TREND"){
      
      what_now=substr(type_analysis,7,nchar(type_analysis))
      

      if(nchar(level_1)>0){
        data_query_res=list()
        data_query_res[[features_query]]=trend_anal_fun_prop(user_id = user_id,session_id = session_id,date_field = date_field,features_query = features_query,what = what_now,pillar = pillar_1,level = level_1,data = data_temp)
        
        
        if(unanticipated_freq_arg=="YES"){
          data_all_res=list()
          data_all_res[[features_query]]=trend_anal_fun_prop(user_id = user_id,session_id = session_id,date_field = date_field,features_query = features_query,what = what_now,pillar = pillar_1,level = level_1,data = data)
          unant_scores=unanticipated_freq_multi_fun_double(data_query_res,data_all_res,counter_now,counter_all)
          
        }else{
          unant_scores=data_query_res
          unant_scores$count_query=counter_now
          unant_scores$count_all=counter_all
        }
        
      }else{
        
        data_query_res=trend_anal_fun(user_id = user_id,session_id=session_id,date_field = date_field,features_query = features_query,what = what_now,data = data_temp)
        if(unanticipated_freq_arg=="YES"){
          data_all_res=trend_anal_fun(user_id = user_id,session_id=session_id,date_field = date_field,features_query = features_query,what = what_now,data = data)
          unant_scores=unanticipated_freq_multi_fun_double(data_query_res,data_all_res,counter_now,counter_all)
          
        }else{
          unant_scores=data_query_res
          unant_scores$count_query=counter_now
          unant_scores$count_all=counter_all
        }
        
      }
      
    }
    
    return(unant_scores)
    
  }else{
    return("No Items matching the query")
  }

  
  
    
  
}

#* Super function for calculating frequencies and unanticipated frequencies withing queries
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param target_feature Data features to inspect (e.g. skills, occupations, location,type). Multiple values are supported and can be separated with the coma (";;"). In cases where the target value is numeric, then you should add a prefix !! and some symbols indicating the direction of the filtering. It should be noted that each data record should have a single value of the selected feature (e.g. upload_date) and not multiple (e.g. skills). Available options: GE (Greater or EQUAL), GR (Greater), LE (Lower or Equal), LO (Lower), EQ (Equal).Example: '!!GE_Height' -> Investigate only the items that have Height greater or equal to the given value (see target_values). Queries of this type can work for non-numeric variables too, e.g., '2024/02/15' > '2023/03/21'. In case the comparisons are made for dates, you should add the prefix !?, followed by the direction (similar to previously), and then the type (year->Y, month->M, year and month->B). Example: "!?LEY_upload_date" -> Investigate only the items that were posted (upload_date) earlier or in the provided year (see target_values).
#* @param target_values Values of the target feature to inspect. Multiple values are supported and can be separated with the coma (";;"). These values match the target_feature arguments 1-1.
#* @param features_query Which features to be inspected for finding unanticipated frequencies with respect to the target values. Multiple values are supported using the seperator ;;, depending on the analysis.
#* @param type_analysis Whether to evaluate pairs or individual entities, or trends. Use 'PAIR' for pairs, and 'ONE' for individual entities, and 'TREND-year', 'TREND-year_month' or 'TREND-month' for trend analysis. 
#* @param pillar_1 Pillars to analyze for the first variable, only used for skills. It can contain multiple pillars separated by ';;'. Available options: Skill, Knowledge, Traversal, Language. It should be empty if propagation is not required.
#* @param level_1 Taxonomy Levels to investigate for the first variable. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ';;' (e.g. 0;;3;;2). It should be empty if propagation is not required
#* @param pillar_2 Pillars to analyze for the second variable, only used for skills. It can contain multiple pillars separated by ';;'. Available options: Skill, Knowledge, Traversal, Language. It should be empty if propagation is not required.
#* @param level_2 Taxonomy Levels to investigate for the second variable. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ';;' (e.g. 0;;3;;2). It should be empty if propagation is not required
#* @param unanticipated_freq_arg Whether to calculate unanticipated frequencies. This has to be either YES or NO.
#* @param type_query Whether to filter the query using logical OR or logical AND. This argument should be either set to 'AND' or 'OR'.
#* @param date_field Variable storing the date within the data. Only used when type_analysis is related to trend analysis

#* @get /frequencies_super_fun
function(user_id,session_id,storage_name,target_feature="",target_values="",features_query,type_analysis,pillar_1="",level_1="",pillar_2="",level_2="",unanticipated_freq_arg,type_query,date_field=""){
  print("Unanticipated frequencies")
  
  
  future({
    
    data_now=frequencies_filtered_call_superfun(user_id=user_id,session_id=session_id,target_feature=target_feature,target_values=target_values,features_query=features_query,type_analysis = type_analysis,pillar_1=pillar_1,pillar_2=pillar_2,level_1=level_1,level_2=level_2,unanticipated_freq_arg=unanticipated_freq_arg,type_query = type_query,date_field=date_field)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'super_freq',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
}



skill_cluster_fun<-function(type_now="kmeans",user_id="1",session_id="1",weight_now='ii_weight',no_clust_now=10,threshold=0.1,umap_nn=5,umap_dim=2,pillar="",level="",vectors_type='weigthing'){
  #data_now=api_ex_now(url,body)
  
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  
  threshold=as.numeric(threshold)
  umap_nn=as.numeric(umap_nn)
  umap_dim=as.numeric(umap_dim)
  no_clust_now=as.numeric(no_clust_now)
  
  if(nchar(pillar)==0){
    
    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        if (length(x$skills) > 0 & !is.null(x$skills)) {
          do.call(
            rbind,
            lapply(x$skills, function(y) {
              return(data.frame(item_id = x$id, skill = y, stringsAsFactors = FALSE))
            })
          )
        }
      })
    )
    
    data_now=double_to_occur(data = data_now)
    
  }else{
    
    data_now=return_double_to_occur_propagate(data = data,what = "skills",pillar = pillar,level=level)
    
  }
  
  no_documents_now=nrow(data_now)
  gc()
  
  #data_now=co_occurence_mat(data_now[,c(1:100)],data_now[,c(1:100)])
  data_now=co_occurence_mat(data_now,data_now)
  gc()
  
  
  if (!(type_now%in%c("correspondence"))&vectors_type=="weighting"){
    diag_values_now=diag(data_now)
    data_now=co_occurence_weight(data = data_now,weight = weight_now,no_documents = no_documents_now)
    gc()
  } 
  
  
  
  if(type_now=='kmeans'){
    
    word_vectors<-word_vectors_method(input_mat = data_now,umap_dim = umap_dim,umap_nn = umap_nn,vectors_type=vectors_type)
    
    clust_output=kmeans_clust_plot(word_vectors = word_vectors,no_clust = no_clust_now,diag_values = diag_values_now)
    
  }else if (type_now=='gmm'){
    
    word_vectors<-word_vectors_method(input_mat = data_now,umap_dim = umap_dim,umap_nn = umap_nn,vectors_type=vectors_type)
    
    clust_output=gmm_clust_plot(word_vectors = word_vectors,no_clust = no_clust_now,diag_values = diag_values_now)
    
  }else if (type_now=='affinity'){
    
    clust_output=affinity_clust_plot(all_pairs_similarity = data_now,diag_values = diag_values_now)
    
  }else if (type_now=='leiden'){
    
    data_now=dichotomize(data_now,thresh = threshold)
    clust_output=leiden_clust_plot(data_now)
    
  }else if (type_now=="correspondence"){
    
    #rownames(data_now)=unlist(data_all_skills$label[match(rownames(data_now),data_all_skills$id)])
    #colnames(data_now)=rownames(data_now)
    
    res.ca=CA(data_now, ncp = no_clust_now, graph = F)
    
    
    clust_output=list(res.ca$eig[,2],res.ca$row$contrib,res.ca$row$coord)
    gc()
    clust_output[[(length(clust_output)+1)]]=unlist(data_all_skills$label[match(colnames(data_now),data_all_skills$id)])
    
    return(clust_output)
    
  }
  
  
  gc()
  
  #clust_output[[1]]=plotly_json(clust_output[[1]])
  clust_output[[1]]=NULL
  clust_output[[1]]$Pref_Label=unlist(data_all_skills$label[match(clust_output[[1]]$Label,data_all_skills$id)])
  return(clust_output)
  
}

#* Cluster analysis based on an algorithm
#* @param type_now The algorithm used for clustering (available  correspondence, kmeans , gmm, leiden, affinity).
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @param weight_now How to measure similarity between skills (ii_weight, mh_weight, ei_weight, ji_weight). Used only when vectors_type is equal to weighting.
#* @param no_clust_now Number of clusters (available only when type_now is equal to correspondence, kmeans or gmm)
#* @param threshold minimum threshold to denote 2 points/skills as neighbors (available only when type_now is equal to leiden)
#* @param umap_nn Parameter of the umap algorithm denoting the no nearest neighbors to be considered when evaluating low-dimensional vectors (available only when type_now is equal to kmeans or gmm)
#* @param umap_dim Parameter of the umap algorithm denotning the no dimensions of the extracted vectors (available only when type_now is equal to kmeans or gmm)
#* @param pillar Pillars to analyze, you must leave this empty if you want to analyze the default skills. It can contain multiple pillars separated by ';;'. Available options: Skill, Knowledge, Traversal, Language.
#* @param level Taxonomy Levels to investigate. Only available when pillar is not empty. One level per pillar should be provided. The levels should be separated by ';;' (e.g. 0;;3;;2).
#* @param vectors_type Ways to project skill vectors. Currently, you can use weigthing (See weight_now) or GloVe. GloVe should be only used when type_now is kmeans or gmm.
#* @get /skillcluster
function(type_now="kmeans",user_id,session_id,storage_name,weight_now='ii_weight',no_clust_now=10,threshold=0.1,umap_nn=5,umap_dim=2,pillar="",level="",vectors_type='weighting') {
  
  print("Skill Clustering")
  
  future({
    
    data_now=skill_cluster_fun(type_now=type_now,user_id=user_id,session_id=session_id,weight_now=weight_now,no_clust_now=no_clust_now,threshold=threshold,umap_nn=umap_nn,umap_dim=umap_dim,pillar=pillar,level=level,vectors_type=vectors_type)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'skill_clust',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
  
}


multi_corresp_fun<-function(user_id="1",session_id="1",no_components=5,features_query="location;;type"){
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  features_query_split=unlist(strsplit(features_query,";;"))
  
  #data_now=api_ex_now(url,body)
  data_now <- do.call(
    rbind,
    lapply(data$items, function(x) {
      items_now=c()
      for (f in features_query_split){
        if(length(x[[f]])!=0 & !is.null(x[[f]])){
          
          items_now[length(items_now)+1]=x[[f]]
          
        }else{
          items_now[length(items_now)+1]=paste("not_found",f,sep="_")
          
        }
      }
      return(items_now)
    }
    )
  )
  
  
  
  
  no_components=as.numeric(no_components)
  res.ca=MCA(X = data_now,ncp = no_components,graph = F)
  #fviz_mca_var(mca_result, repel = TRUE)
  
  
  
  outputs_list=list(
    
    as.data.frame(res.ca$var$contrib),
    as.data.frame(res.ca$var$coord)
    
  )
  
  return(outputs_list)
  
}

#* Multiple Correspondence analysis (cannot use skills or any other multi-value characteristic/variable)
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param no_components number of components of the final model
#* @param features_query which features_query to be extracted from the imported data. They should be separated using the character ';;' (e.g. location,type). Each feature should have a unique value (e.g. upload date) and not multiple (e.g. skills)
#* @param storage_name Store the outcome of the analysis with a unique code name. This utility enables the existence of multiple outputs for each type of analysis. Should not be empty!
#* @get /multicorr
function(user_id="1",session_id="1",storage_name="",no_components=5,features_query="location;;type"){
  
  print("Multiple Correspondence Analysis")
  
  
  future({
    
    data_now=multi_corresp_fun(user_id=user_id,session_id=session_id,no_components=no_components,features_query=features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,variable_name = 'multiple_cor',variable_value = data_now,subvariable_name = storage_name)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
}






