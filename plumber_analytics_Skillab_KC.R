

#path_user_files_all="C:/Users/kostas charm/Documents/Skillab/analytics_microservice/"
path_user_files_all="~/"


library(plumber)
library(future)
library(promises)
library(igraph)
library(ggplot2)
library(plotly)
library(ggthemes)
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



###API example one page
api_ex_now_one<-function(url,body=list()){
  
  #API default
  #http://skillab-tracker.csd.auth.gr/api/docs#/Job/api_views_get_jobs
  
  #Define the API endpoint and parameters
  #url <- "http://skillab-tracker.csd.auth.gr/api/jobs?page=1"
  #url <- "http://skillab-tracker.csd.auth.gr/api/law-publications?page=1"
  
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
api_ex_now<-function(url,body=list(),per_page=300){
  
  #url <- "http://skillab-tracker.csd.auth.gr/api/jobs"
  #body <- 'keywords=seller&keywords=market'
  #per_page <- 300
  
  #Exmaples body and url
  #url=paste0("http://skillab-tracker.csd.auth.gr/api/skills",page)
  #body=list(min_skill_level=1) OR body="min_skill_level=1"
  
  #Define parameters
  page=1

  # Load the libraries

  
  
  url_now=paste0(url,"?page=",page)
  
  # Make the POST request
  response <- POST(
    url_now,
    add_headers(
      `accept` = "application/json",
      `Content-Type` = "application/x-www-form-urlencoded"
    ),
    body = body,
    encode = "form"
  )
  

  
  items <- as.list(content(response, "parsed"))
  print("PAGE 1")
  
  items$endpoint=url_now
  items$query=body
  
  if(items$count>0&items$count>=per_page){
    
    
    pages_to_collect=items$count%/%per_page
    mod_now=items$count %%per_page
    if (mod_now!=0)pages_to_collect=pages_to_collect+1
    
    for (page in 2:pages_to_collect){
      
      url_now=paste0(url,"?page=",page)
      
      
      print(paste("PAGE",page))
      # Make the POST request
      response <- POST(
        url_now,
        add_headers(
          `accept` = "application/json",
          `Content-Type` = "application/x-www-form-urlencoded"
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
url="http://skillab-tracker.csd.auth.gr/api/skills"
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
url="http://skillab-tracker.csd.auth.gr/api/occupations"
#body="min_level=1"

print("Loading Occupation Data")
data_all_occupations=api_ex_now(url)
data_all_occupations <- do.call(rbind, lapply(data_all_occupations$items, flatten_list_occupations))
data_all_occupations <- as.data.frame(data_all_occupations, stringsAsFactors = FALSE)

gc()


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


####Matrix for doubles of skills and ancestors
double_to_occur_with_propagate<-function(data,table_now){
  
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
  
  for(i in 1:nrow(data)){
    
    #print(i)
    
    item_pos=data[i,1]
    skill_pos=data[i,2]
    
    new_pos=as.numeric(which(table_now[,1]==skill_pos))
    new_pos=table_now[new_pos,2]
    
    binary_matrix[item_pos,new_pos]=1
    
  }
  
  return(binary_matrix)
  
}

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
propagation_skills <-function(data,urls,pillar="Skill,Language",level="1,2"){
  
  
  pillar_split=unlist(strsplit(pillar,","))
  level_split=as.numeric(unlist(strsplit(level, ",")))
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
  
  
  l=0
  
  #x=unlist(data_all_skills$id[14])
  
  link_skill=do.call(
    
    rbind,
    lapply(urls,function(x){
      
      l=l+1
      
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
          
          levels_which=which(unlist(temp_levels)>level_split[t])
          
          if(length(levels_which)!=0){
            

            ancestors_now=c(ancestors_now,unlist(lapply(temp_ancestors[levels_which],function(x){
              

              x=unlist(x)
                
              lvl_now=length(x)-level_split[t]

              return(x[[lvl_now]])
                
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



###Example double to occur with propagation in skills
double_to_occur_with_propagation_skills_example <-function(){
  
  
  url="http://skillab-tracker.csd.auth.gr/api/jobs"
  body="keywords=seller&keywords=market"
  
  data =  api_ex_now(url,body)
  #data_all=data
 
  pillar="";level=""
  #pillar="Skill,Knowledge,Language,Traversal";level="1,1,2,1";  binary_mat_double_to_occur=return_double_to_occur_propagate(what = "skills",pillar = pillar,level = level)
  

  
  if(nchar(pillar)==0){
    
    binary_mat_double_to_occur <- do.call(
      rbind,
      lapply(data$items, function(x) {
        if (length(x$skills) > 0) {
          do.call(
            rbind,
            lapply(x$skills, function(y) {
              return(data.frame(item_id = x$id, skill = y, stringsAsFactors = FALSE))
            })
          )
        }
      })
    )
    
    binary_mat_double_to_occur=double_to_occur(data = binary_mat_double_to_occur)
  
  }

  

  
  #urls=unique(unlist(data_now[,2]))
  
  
  
  
  
  ###Descriptive
  {
    
    col_sums=colSums(binary_mat_double_to_occur)
    col_sums=data.frame(names(col_sums),as.numeric(col_sums))
    
    col_sums[,1]=unlist(data_all_skills$label[match(col_sums[,1],data_all_skills$id)])
    col_sums=col_sums[order(col_sums[,2],decreasing = T),]
    
  }

  
  
  ###Clustering
  {
    
    
    type_now="kmeans"
    weight_now='ii_weight'
    no_clust_now=5
    threshold=0.1
    umap_nn=5
    umap_dim=2
    vectors_type='weighting' # weighting or GloVe

    no_documents_now=nrow(binary_mat_double_to_occur)
    gc()
    
    #data_now=co_occurence_mat(data_now[,c(1:100)],data_now[,c(1:100)])
    data_now=co_occurence_mat(binary_mat_double_to_occur,binary_mat_double_to_occur)
    
    if(vectors_type=="weighting"){
      
      data_now=co_occurence_weight(data = data_now,weight = weight_now,no_documents = no_documents_now)
      
    }
    
    gc()
    
    
    if(type_now=="gmm"){
      
      word_vectors=word_vectors_method(data_now,umap_nn = umap_nn,umap_dim = umap_dim,vectors_type = vectors_type)
      clust_output=gmm_clust_plot(word_vectors = word_vectors,no_clust = no_clust_now,diag_values = diag(data_now))
      #print(table(clust_output[[2]][["Cluster"]]))
      
    }else if (type_now=="kmeans"){
      
      word_vectors=dimensionality_reduction_method(data_now,umap_nn = umap_nn,umap_dim = umap_dim,vectors_type = vectors_type)
      clust_output=kmeans_clust_plot(word_vectors = word_vectors,no_clust = no_clust_now,diag_values = diag(data_now))
      #print(table(clust_output[[2]][["Cluster"]]))
      
    }else if (type_now=="affinity"){
      
      clust_output=affinity_clust_plot(all_pairs_similarity = data_now,diag_values = diag(data_now))
      
    }

    
    gc()
    
    clust_output[[1]]=NULL
    clust_output[[1]]$Pref_Label=unlist(data_all_skills$label[match(clust_output[[1]]$Label,data_all_skills$id)])
    
    
  }

  
}



###Propagation Occupations
propagation_occupations <-function(data,urls,level="1"){
  
  level=as.numeric(level)
  
  column_levels="levels"
  column_ancestors="ancestors"
  
  
  l=0
  
  link_occupation=do.call(
    
    rbind,
    lapply(urls,function(x){
      
      l=l+1
      
      pos_now=match(x,data_all_occupations$id)
      
      temp_levels=unlist(data_all_occupations[[column_levels]][pos_now][[1]])
      temp_ancestors=data_all_occupations[[column_ancestors]][pos_now][[1]]
      
      
      if(length(temp_levels)!=0){
        
        levels_which=which(unlist(temp_levels)>=level)
        
        if(length(levels_which)!=0){
          
          k=0
          
          ancestors_now=unlist(lapply(temp_ancestors[levels_which],function(x){
            k=k+1
            if(levels_which[k]==level){
              return(urls[l])
            }else{
              
              x=unlist(x)
              lvl_now=length(x)-level
              if(lvl_now==0)lvl_now=1
              
              return(x[[lvl_now]])
              
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


###Example double to occur with propagation in occupations
double_to_occur_with_propagation_occupations_example <-function(){
  
  url="http://skillab-tracker.csd.auth.gr/api/jobs"
  body="keywords=seller&keywords=market"
  
  data =  api_ex_now(url,body)
  #data_all=data
  
  data_now <- do.call(
    rbind,
    lapply(data$items, function(x) {
      if (length(x$occupations) > 0) {
        do.call(
          rbind,
          lapply(x$occupations, function(y) {
            return(data.frame(item_id = x$id, skill = y, stringsAsFactors = FALSE))
          })
        )
      }
    })
  )
  
  
  level=2
  urls=unique(unlist(data_now[,2]))
  
  table_now=propagation_occupations(urls,level = level)
  
  
  binary_mat_double_to_occur=double_to_occur_with_propagate(data_now,table_now)
  
  col_sums=colSums(binary_mat_double_to_occur)
  col_sums=data.frame(names(col_sums),as.numeric(col_sums))
  
  col_sums[,1]=unlist(data_all_occupations$label[match(col_sums[,1],data_all_occupations$id)])
  col_sums=col_sums[order(col_sums[,2],decreasing = T),]
  
  
  
  
}


###Return double to occur with propagation
return_double_to_occur_propagate<-function(data,what,pillar,level,is_mat=T){
  
  data_now <- do.call(
    rbind,
    lapply(data$items, function(x) {
      if (length(x[[what]]) > 0) {
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
    table_now=propagation_occupations(data,urls = unlist(data_now[,2]),level = level)
    
  }else if (what=="skills"){
    table_now=propagation_skills(data,urls = unlist(data_now[,2]),pillar = pillar,level = level)
    
  }
  
  if (is_mat){
    
    return(double_to_occur_with_propagate(data_now,table_now))
    
  }else{
    return(double_to_double_with_propagate(data_now,table_now))
    
  }
  
  

}


#####Co-occurence mat
co_occurence_mat<-function(matrix1,matrix2){
  return(t(matrix1) %*% matrix2)
}

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


####Clustering algorithms
leiden_clust_plot <- function(skill_co_mat){

  
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
    
    
    palete_col=rainbow(n = (l_clust_final$nb_clusters+1))
    
    
    all_df$cluster=as.character(all_df$cluster)
    
    ppl= ggplotly(
      ggplot()+
        ggtitle(paste("No clusters:",l_clust_final$nb_clusters))+
        geom_segment(data=g_matrix,aes(x=from.x,xend = to.x, y=from.y,yend = to.y),colour="black") + #size="weight"
        geom_point(data=all_df,aes(x=V1,y=V2,label=Label,colour=cluster),size=5) +#(palete_col[as.numeric(all_df$cluster)+1])
        #geom_text(data=all_df,aes(x=as.numeric(V1),y=as.numeric(V2),label=paste(all_df$cluster,all_df$Label)))+
        #theme_solarized_2(light = F)
        theme(
          panel.background = element_rect(fill = "white",
                                          colour = "lightblue",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "lightgrey"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "lightgrey")
        )+
        #theme_void()+
        theme_solarized()+
        xlab("")+
        ylab("")
    )
    
    return(list(ppl,all_df,g_matrix))
    
  }
}


affinity_clust_plot <-function(all_pairs_similarity,diag_values){

  
  
  
  ap_clust=apcluster(all_pairs_similarity)
  
  clust=matrix(ncol=1,nrow = nrow(all_pairs_similarity),0)
  for(i in 1:length(ap_clust@clusters)){
    clust[ap_clust@clusters[[i]],1]=i
  }
  
  g_matrix=matrix(nrow = (nrow(all_pairs_similarity)),ncol=2)
  for(i in 1:nrow(all_pairs_similarity)){
    g_matrix[i,]=c(i,ap_clust@exemplars[clust[i,1]])
    
  }
  
  
  g_matrix=graph_from_data_frame(g_matrix)
  set.seed(31345)
  new_map=layout.fruchterman.reingold(g_matrix)*0.5
  
  colnames(new_map)=c("x","y")
  
  rownames(new_map)=rownames(all_pairs_similarity)
  
  
  
  ###Points
  df=as.data.frame(new_map)
  #df$label=paste(rownames(all_pairs_similarity),diag_values)
  df$Label=rownames(all_pairs_similarity)
  df$cluster=clust[,1]
  #df$freq=diag_values
  
  ###Segments
  g_matrix=matrix(nrow = (nrow(df)),ncol=3)
  for(i in 1:nrow(new_map)){
    g_matrix[i,]=c(new_map[ap_clust@exemplars[clust[i]],],clust[i])
    
  }
  g_matrix=cbind(new_map[,2],g_matrix)
  g_matrix=cbind(new_map[,1],g_matrix)
  g_matrix=g_matrix[-ap_clust@exemplars,]
  g_matrix=as.data.frame(g_matrix)
  g_matrix=as.data.frame(g_matrix)
  colnames(g_matrix)=c("from.x","from.y","to.x","to.y","Label")
  
  
  palete_col=rainbow(n = length(ap_clust@exemplars))
  
  #df$Frequency=diag(tag_co_matrix)[included_tags()]
  
  df$cluster=as.character(df$cluster)
  
  new_clust_plot <- ggplotly(ggplot(data = df, aes(x,y,label=Label,colour=cluster)) +
                               geom_point(size=2)+#(colour=cluster,size=1)+#palete_col[df$cluster]
                               
                               geom_text(data=df[ap_clust@exemplars,],aes(x,y,label=Label))+
                               geom_segment(data=g_matrix,aes(x=from.x,xend = to.x, y=from.y,yend = to.y)
                                            ,colour=palete_col[g_matrix$Label],size=0.1)+
                               #theme_solid()
                               #theme_ipsum_ps()
                               #theme_solarized_2()+
                               theme_solarized()+
                               #theme_void()+
                               xlab("")+
                               ylab("")
  )

  
  
  return(list(new_clust_plot,df,data.frame(Label=rownames(df)[ap_clust@exemplars],pos=ap_clust@exemplars)))
  
}


word_vectors_method<-function(input_mat,umap_nn=5,umap_dim=2,vectors_type="GloVe"){
  
  

  
  set.seed(123)
  
  if(vectors_type=="weighting"){
    
    word_vectors=umap(as.dist(input_mat),n_neighbors = umap_nn,n_components = umap_dim,verbose = T)
    
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


kmeans_clust_plot <-function(word_vectors,no_clust=10,diag_values,labeled_legend=F){
  

  
  
  set.seed(831)
  
  clust_now=kmeans(x = word_vectors,centers = no_clust)
  
  
  word_vectors$Cluster=as.numeric(unlist(clust_now$cluster))
  word_vectors$Label=rownames(word_vectors)
  
  
  if(labeled_legend==T){ 
    clust_labels=c()
    for (i in 1:length(table(word_vectors))){
      matched_now=which(word_vectors$Cluster==i)
      
      order_now=order(diag_values[matched_now],decreasing = T)
      
      if(length(matched_now)>5){
        
        clust_labels[i]=paste(word_vectors$Label[matched_now][order_now[1:3]],collapse = " ; ")#
        
      }else{
        
        clust_labels[i]=paste(word_vectors$Label[matched_now],collapse = " ; ")
        
      }
      
      
    }
    
    word_vectors$Cluster=clust_labels[word_vectors$Cluster]
    
  }else{
    word_vectors$Cluster=as.character(unlist(clust_now$cluster))
    
  }
  
  colnames(word_vectors)[c(1,2)]=c("x","y")
  
  
  ppl2 = 
    ggplotly(
      ggplot(data = word_vectors, mapping = aes(x = x, y = y, colour = Cluster, label = Label)) +
        geom_point() +
        labs(colour = "Cluster", x = "", y = "") +
        ggtitle("Cluster Plot") +
        theme_solarized()
      
    )
  ppl2 <- ppl2 %>%
    layout(legend = list(orientation = "h",   # Horizontal legend
                         x = 0.5,             # Centered horizontally
                         y = -0.2,            # Below the plot
                         xanchor = "center",  # Align legend by its center
                         yanchor = "top"))    # Align legend by its top
  
  
  return(list(ppl2,word_vectors))
  
}


gmm_clust_plot <-function(word_vectors,no_clust=10,diag_values,labeled_legend=F){
  



  
  set.seed(831)
  
  clust_now=Mclust(word_vectors,G=no_clust,verbose = TRUE) 
  word_vectors$Cluster=as.numeric(unlist(clust_now$classification))
  word_vectors$Label=rownames(word_vectors)
  
  
  if (labeled_legend==T){
    
    
    clust_labels=c()
    for (i in 1:length(table(word_vectors))){
      matched_now=which(word_vectors$Cluster==i)
      
      order_now=order(diag_values[matched_now],decreasing = T)
      
      if(length(matched_now)>5){
        clust_labels[i]=paste(word_vectors$Label[matched_now][order_now[1:3]],collapse = " ; ")
      }else{
        clust_labels[i]=paste(word_vectors$Label[matched_now],collapse = " ; ")
        
      }
      
      
    }
    
    
    word_vectors$Cluster=clust_labels[word_vectors$Cluster]
    
    
    
  }else{
    word_vectors$Cluster=as.character(clust_now$classification)
    
  }
  
  colnames(word_vectors)[c(1,2)]=c("x","y")
  
  ppl2 = 
    ggplotly(
      ggplot(data = word_vectors, mapping = aes(x = x, y = y, colour = Cluster, label = Label)) +
        geom_point() +
        labs(colour = "Cluster", x = "", y = "") +
        ggtitle("Cluster Plot") +
        theme_solarized()
      
    )
  ppl2 <- ppl2 %>%
    layout(legend = list(orientation = "h",   # Horizontal legend
                         x = 0.5,             # Centered horizontally
                         y = -0.2,            # Below the plot
                         xanchor = "center",  # Align legend by its center
                         yanchor = "top"))    # Align legend by its top
  
  
  return(list(ppl2,word_vectors))
}


load_user_session_file<-function(user_id="1",session_id="1",attribute="none"){
  
  
  user_folder=paste0("user_",user_id)
  session_file=paste0("session_",session_id)
  
  if(attribute=="none"){
    return(readRDS(paste0(path_user_files_all,user_folder,"/",session_file)))
    
  }else{
    return(readRDS(paste0(path_user_files_all,user_folder,"/",session_file))[[attribute]])
    
  }
  
}

save_update_user_session_file<-function(user_id="1",session_id="1",variable_name="query",variable_value="keywords=market"){
  
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
    data[[variable_name]]=variable_value
    
    
  }else{
    
    user_files=list.files(paste0(path_user_files_all,user_folder,"/"))
    
    find_session=match(session_file,user_files)
    
    if(is.na(find_session)){
      
      data=list()
      data[[variable_name]]=variable_value

      
    }else{
      
      data=readRDS(paste0(path_user_files_all,user_folder,"/",session_file))
      data[[variable_name]]=variable_value

    }
    
  }
  saveRDS(data,paste0(path_user_files_all,user_folder,"/",session_file))
  
}



#* @apiTitle Skillab analytics and skill clustering
#* @apiDescription Conduct descriptive and exploratory analytics on the various data fields. In addition, functionalities for skill clustering and correspondence analysis are supported as well.


#* Load Data from API
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param url Url of the endpoint (e.g. http://skillab-tracker.csd.auth.gr/api/jobs)
#* @param body Body for the POST API call (e.g. keywords=seller&keywords=market)
#* @get /load_data
function(url="",body="",user_id="",session_id=""){
  
  print("Loading Data")
  
  future({
    
    data_now<-api_ex_now(url, body)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'data',data_now)
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
#* @param attribute The attribute to return. Current options: data - Returns all data, all_stats - Returns output from Descriptive statistics,  all_stats_prop - Returns output from Descriptive statistics with propagation, explor_stats - Returns output from Exploratory Analytics, trend_anal - Returns output from Trend Analysis, skill_clust - Returns output from  Cluster analysis, multiple_cor - Returns output from Multiple Correspondence analysis
#* @get /get_data
function(user_id,session_id,attribute){
  future({
    return(load_user_session_file(user_id,session_id,attribute))
    
  }) %...>%{  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
}

analytics_fun<-function(user_id="1",session_id="1",features_query=""){
  
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  #data_now=api_ex_now(url,body)
  
  features_query_split=unlist(strsplit(features_query,","))
  
  data_now_list=list()
  
  for (f in features_query_split){
    
    if(f %in% c("skills","occupations")){
      
      data_now_list[[f]]= unlist(
        lapply(data$items, function(x) {
          if (length(x[[f]]) > 0) {
            
            unlist(lapply(x[[f]], function(y) {
              y
            }))
            
          }
        })
      )
      
      data_now_list[[f]]=as.data.frame(sort(table(data_now_list[[f]]),decreasing = T))
      
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
      
    }else{
      
      data_now_list[[f]]=unlist(lapply(data$items,function(x){
        
        if(length(x[[f]])!=0){
          data_now_list[[f]][length(data_now_list[[f]])+1]=x[[f]]
          
        }else{
          data_now_list[[f]][length(data_now_list[[f]])+1]=paste("not_found",f,sep="_")
          
        }
      }
      ))
      data_now_list[[f]]=as.data.frame(sort(table(data_now_list[[f]]),decreasing = T))
      
    }
    
    
  }
  
  gc()
  
  return(data_now_list)
  
}

#* Descriptive statistics
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param features_query Which features_query to be extracted from the imported data. They should be separated using the character ',' (e.g. skills, occupations, location,type)
#* @get /analytics_descriptive
function(user_id,session_id,features_query=""){
 
  print("Descriptive statistics")
  
  future({
    
    data_now=analytics_fun(user_id = user_id, session_id = session_id,features_query = features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'all_stats',data_now)
    return(data_now)
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
}


analytics_prop_fun<-function(user_id="1",session_id="1",what='skills',pillar="Skill,Language",level="1,2"){
  
  #data_now=api_ex_now(url,body)
  
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  
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
#* @param what What to investigate. This has to be either skills or occupations
#* @param pillar Pillars to analyze, only used for skills. It can contain multiple pillars separated by ','. Available options: Skill, Knowledge, Traversal, Language.
#* @param level Taxonomy Levels to investigate. Single value for occupations or one level per pillar for skills should be provided. The levels should be separated by ',' (e.g. 0,3,2).
#* @get /analytics_descriptive_propagation
function(user_id="1",session_id="1",what='skills',pillar="Skill,Language",level="1,2"){
  
  print("Descriptive statistics with propagation")
  
  future({
    
    data_now=analytics_prop_fun(user_id = user_id, session_id = session_id,what=what,pillar=pillar,level=level)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'all_stats_prop',data_now)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
}





exploratory_fun<-function(user_id="1",session_id="1",features_query=""){
  
  
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  
  #data_now=api_ex_now(url,body)
  features_query_split=unlist(strsplit(features_query,","))
  
  if(features_query_split[1] %in% c("skills","occupations") & features_query_split[2] %in% c("skills","occupations")){
    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        
        if (length(x[[features_query_split[[1]]]]) > 0) {
          if(length(x[[features_query_split[[2]]]]) > 0) {
            
            do.call(
              rbind,
              lapply(x[[features_query_split[1]]], function(y) {
                do.call(
                  rbind,
                  lapply(x[[features_query_split[2]]], function(z) {
                    
                    return(data.frame(item_1 = y, item_2 = z, stringsAsFactors = FALSE))
                    
                  })
                )
              })
            )
            
          }
        }
      })
    )
    
  }else if (features_query_split[1] %in% c("skills","occupations") ){
    
    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        
        if (length(x[[features_query_split[[1]]]]) > 0) {
          if(length(x[[features_query_split[[2]]]]) > 0) {
            
            do.call(
              rbind,
              lapply(x[[features_query_split[1]]], function(y) {
                
                return(data.frame(item_1 = y, item_2 = x[[features_query_split[2]]], stringsAsFactors = FALSE))
                
                
                
              })
            )
            
          }
        }
      })
    )
    
  }else if (features_query_split[2] %in% c("skills","occupations")|features_query_split[1] %in% c("skills","occupations") ){
    
    if(features_query_split[1]%in%c("skills","occupations")){
      l_2=features_query_split[1]
      l_1=features_query_split[2]
    }else{
      l_2=features_query_split[2]
      l_1=features_query_split[1]
    }
    
    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        
        if (length(x[[l_1]]) > 0) {
          if(length(x[[l_2]]) > 0) {
            
            do.call(
              rbind,
              lapply(x[[l_2]], function(y) {
                
                return(data.frame(item_1 = y, item_2 = x[[l_1]], stringsAsFactors = FALSE))
                
                
                
              })
            )
            
          }
        }
      })
    )
    
  }else{
    
    data_now <- do.call(
      rbind,
      lapply(data$items, function(x) {
        
        if (length(x[[features_query_split[[1]]]]) > 0) {
          if(length(x[[features_query_split[[2]]]]) > 0) {
            
            return(data.frame(item_1 = x[[features_query_split[[1]]]], item_2 = x[[features_query_split[2]]], stringsAsFactors = FALSE))
            
          }
        }
        
      })
    )
    
    
  }
  
  
  data_now=as.data.frame(table(data_now[,1],data_now[,2]))
  data_now=data_now[-which(data_now$Freq ==0 ),]
  data_now=data_now[order(data_now$Freq,decreasing = T),]
  return(data_now)
  
}

#* Exploratory Analytics
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param features_query Which features_query to be extracted from the imported data. They should be exactly two features separated using the character ',' (e.g. skills, occupations, location,type)
#* @get /analytics_exploratory
function(user_id="1",session_id="1",features_query=""){
  
  print("Exploratory statistics")
  
  future({
    
    data_now=exploratory_fun(user_id = user_id, session_id = session_id,features_query = features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'explor_stats',data_now)
    return(data_now)
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
 
  
}



trend_anal_fun<-function(user_id="1",session_id="1",date_field="upload_date",features_query="",date_format="%Y-%m-%d",what="year"){
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  features_query_split=unlist(strsplit(features_query,","))
  
  if(what=="year"){
    digits_to_see_start=1
    digits_to_see_end=4
  }else if (what=="month"){
    digits_to_see_start=5
    digits_to_see_end=7
  }else if (what=="year_month"){
    digits_to_see_start=1
    digits_to_see_end=7
  }
  
  dates_now=lapply(data$items,function(x){
    temp_now=unlist(strsplit(x[[date_field]]," "))[[1]]
    temp_now=as.Date(x = temp_now,format=date_format)
    temp_now=substr(temp_now,start=digits_to_see_start,stop = digits_to_see_end)
    return(temp_now)
  })
    
  dates_now=unlist(as.matrix(dates_now,ncol=1))
  #rownames(dates_now)=NULL
  
  data_now=list()
  for (f in features_query_split){
    
    if(f %in% c("skills","occupations")){
      
      data_now[[f]] <- do.call(
        rbind,
        lapply(c(1:length(dates_now)), function(i) {
          
          if (length(data$items[[i]][[f]]) > 0) {
              do.call(
                rbind,
                lapply(c(1:length(data$items[[i]][[f]])), function(j) {
                  
                  return(data.frame(date = dates_now[i], item = data$items[[i]][[f]][[j]], stringsAsFactors = FALSE))
                  
                })
              )
              
            
          }
        })
      )
      
      data_now[[f]]=as.data.frame(table(data_now[[f]]))
      data_now[[f]]=data_now[[f]][-which(data_now[[f]]$Freq==0),]
      
    }else{
      
      data_now[[f]] <- do.call(
        rbind,
        lapply(c(1:length(dates_now)), function(i) {
          
          return(data.frame(date = dates_now[i], item = data$items[[i]][[f]], stringsAsFactors = FALSE))
          
        })
      )
      
      data_now[[f]]=as.data.frame(table(data_now[[f]]))
      data_now[[f]]=data_now[[f]][-which(data_now[[f]]$Freq==0),]
      
    }
    
  }
  
  return(data_now)
}


#* Trend analysis
#* @param user_id The id of the user
#* @param session_id The id session of the user's current session
#* @param features_query Which features_query to be extracted from the imported data. They should be separated using the character ',' (e.g. skills, occupations, location,type)
#* @param date_format The date format of the stored data. Example (2025-01-02): "%Y-%m-%d"
#* @param what What to investigate regarding dates-trends. Possible values: 'year' , 'month' , 'year_month'.
#* @get /trend_analysis
function(user_id="1",session_id="1",date_field="upload_date",features_query="location,type",date_format="%Y-%m-%d",what="year"){
  print("Trend Analysis")
  
  
  future({
    
    data_now=trend_anal_fun(user_id=user_id,session_id=session_id,date_field=date_field,features_query=features_query,date_format=date_format,what=what)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'trend_anal',data_now)
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
        if (length(x$skills) > 0) {
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
    
    data_now=return_double_to_occur_propagate(what = "skills",pillar = pillar,level=level)
    
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
#* @param weight_now How to measure similarity between skills (ii_weight, mh_weight, ei_weight, ji_weight). Used only when vectors_type is equal to weighting.
#* @param no_clust_now Number of clusters (available only when type_now is equal to correspondence, kmeans or gmm)
#* @param threshold minimum threshold to denote 2 points/skills as neighbors (available only when type_now is equal to leiden)
#* @param umap_nn Parameter of the umap algorithm denoting the no nearest neighbors to be considered when evaluating low-dimensional vectors (available only when type_now is equal to kmeans or gmm)
#* @param umap_dim Parameter of the umap algorithm denotning the no dimensions of the extracted vectors (available only when type_now is equal to kmeans or gmm)
#* @param pillar Pillars to analyze, you must leave this empty if you want to analyze the default skills. It can contain multiple pillars separated by ','. Available options: Skill, Knowledge, Traversal, Language.
#* @param level Taxonomy Levels to investigate. Only available when pillar is not empty. One level per pillar should be provided. The levels should be separated by ',' (e.g. 0,3,2).
#* @param vectors_type Ways to project skill vectors. Currently, you can use weigthing (See weight_now) or GloVe. GloVe should be only used when type_now is kmeans or gmm.
#* @get /skillcluster
function(type_now="kmeans",user_id="1",session_id="1",weight_now='ii_weight',no_clust_now=10,threshold=0.1,umap_nn=5,umap_dim=2,pillar="",level="",vectors_type='weighting') {
  
  print("Skill Clustering")
  
  future({
    
    data_now=skill_cluster_fun(type_now=type_now,user_id=user_id,session_id=session_id,weight_now=weight_now,no_clust_now=no_clust_now,threshold=threshold,umap_nn=umap_nn,umap_dim=umap_dim,pillar=pillar,level=level,vectors_type=vectors_type)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'skill_clust',data_now)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
  
}


multi_corresp_fun<-function(user_id="1",session_id="1",no_components=5,features_query="location,type"){
  data<-load_user_session_file(user_id = user_id,session_id = session_id)$data
  
  features_query_split=unlist(strsplit(features_query,","))
  
  #data_now=api_ex_now(url,body)
  data_now <- do.call(
    rbind,
    lapply(data$items, function(x) {
      items_now=c()
      for (f in features_query_split){
        if(length(x[[f]])!=0){
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
#* @param features_query which features_query to be extracted from the imported data. They should be separated using the character ',' (e.g. location,type)
#* @get /multicorr
function(user_id="1",session_id="1",no_components=5,features_query="location,type"){
  
  print("Multiple Correspondence Analysis")
  
  
  future({
    
    data_now=multi_corresp_fun(user_id=user_id,session_id=session_id,no_components=no_components,features_query=features_query)  # Simulate the API call
    save_update_user_session_file(user_id = user_id,session_id = session_id,'multiple_cor',data_now)
    return(data_now)
    
  }) %...>% {  # On success
    
    data_now <- . 
    
    
    return(data_now)
    
  } %...!% (function(error) {  # On error (Parentheses added here)
    paste("Error loading data:", error$message)
  })
  
  
}






