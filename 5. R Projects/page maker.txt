

# (Updated 12/11) How I Generate This Webpage

I generate this site from a directory of markdown files using a script written in R. The code for this and some instructions are given below.


## Set up your directory.

Your directory should have a folder called "markdown". This folder must contain one folder for each category of page you want on the index page of the site. I have, for instance "papers" and "teaching" as subfolders. Each subfolder must have one markdown file per page you want on the site. The markdown files should start with a single blank line and then a header `# Title`. This header will be used as the title of the page on the index.

The markdown folder should also contain a file called header.md which will be placed on top of every page (except the index). Mine is just:

```
[Back](../index.html)
```

The markdown folder should also contain a file called indexHeader.md which will be placed at the top of the index. Mine is just some ascii art:

```
   ,___             __          ############
  /   /            ( /          # Greg Leo #
 /  ___   _  _,     /    _  __  # VU Econ  #
 ___// (_(/_(_)_  (/___/(/_(_)  # Theory   #
             /|                 # Exp.     #
            (/                  ############
```

Place a stylesheet (css) file in the directory in a /style subdirectory. The content of my stylesheet is provided below. Feel free to use that as a base.

## Running the code.

To generate your site, run the following code. Set your working directory to the path containing all the folders you set up above. Also make sure to update the page title!

```{r,running,eval=FALSE,echo=TRUE}
library(knitr)
library(fs)
library(stringr)
library(dplyr)
library(stringi)
library(magrittr)

list.dirs <- function(path = ".",
                      pattern = NULL,
                      all.dirs = FALSE,
                      full.names = FALSE,
                      ignore.case = FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path,
                    pattern,
                    all.dirs,
                    full.names = TRUE,
                    recursive = FALSE,
                    ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if (isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

#Get Folders in Markdown Directory
getFolders <- function(directory) {
  markdownDirectory = paste(c(directory, "/markdown"), collapse = "")
  folders = list.dirs(markdownDirectory)
}

#Get Files in Markdown Directory Folder
getFiles <- function(folder, directory) {
  markdownDirectory = paste(c(directory, "/markdown"), collapse = "")
  staticDirectory = paste(c(directory, "/static"), collapse = "")
  currentDirectoryMarkdown = paste(c(markdownDirectory, "/", folder), collapse =
                                     "")
  currentDirectory = paste(c(staticDirectory, "/", folder), collapse =
                             "")
  temp = list.files(currentDirectoryMarkdown, pattern = "*.md$")
  temp
}

#Extract Page Title from File
getTitle <- function(file) {
  pageTitle <- str_extract(readChar(file, 500), regex("# .*"))
  pageTitle <- substring(pageTitle, 3)
  pageTitle
}


#Extract Page Title from File
getPriority <- function(file) {
  priority <- str_extract(readChar(file, 10), regex("[0-9]{1,10}"))
  priority
}


#Add a "back" link to each page.
addBack <- function(fileInfo){
  content <- fileInfo$content
  content <- paste("[Back](../index.html)  \n\n\n", content, collapse = "")
  fileInfo$content <- content
  fileInfo
}

#Setup the Static Folder
makeStaticFolder <- function(folder, directory) {
  staticDirectory = paste(c(directory, "/static"), collapse = "")
  currentDirectory = paste(c(staticDirectory, "/", folder), collapse =
                             "")
  dir.create(currentDirectory)
}
setupStatic <- function(directory, folders) {
  sapply(folders, makeStaticFolder, directory = directory)
}

#Setup the Temp Folder
setupTemp <- function(directory) {
  tempDirectory = paste(c(directory, "/temp"), collapse = "")
  dir.create(tempDirectory)
}

#Get Full Path to Markdown File
getMarkdownPath <- function(file, folder, directory) {
  paste(c(directory, "/markdown/", folder, "/", file), collapse = "")
}

#Get Full Path to Static File
getStaticPath <- function(file, folder, directory) {
  paste(c(
    directory,
    "/static/",
    folder,
    "/",
    substr(file, 1, nchar(file) - 2),
    "html"
  ),
  collapse = "")
}

#Make a Link to the Static File in Markdown Format
makeLink <- function(fileInfo) {
    paste(c(
    "[",
    fileInfo$title,
    "](",
    fileInfo$folder,
    "/",
    substr(as.character(fileInfo$fileName), 1, nchar(as.character(fileInfo$fileName)) - 2),
    "html)\n"
  ),
  collapse = "")
}

#Create the Index Page
makeIndex <- function(fileData,pageTitle,indexHeader) {

  indexHeaderPath <-  paste(directory, "/markdown/indexHeader.md", sep = "")
  indexHeader <-  readChar(indexHeaderPath, file.info(indexHeaderPath)$size)
  indexText <- indexHeader
  folders <- fileData %>% pull(folder) %>% unique
  for(currentFolder in folders){

    indexText <- c(indexText,paste("## ",toupper(substring(currentFolder,3)),sep=""))
    subsetFiles <- fileData %>% filter(folder==currentFolder) %>% arrange(desc(priority))
    for(i in 1:dim(subsetFiles)[1]){
      print(subsetFiles[i,])
      indexText <- c(indexText,makeLink(subsetFiles[i,]))
    }
  }
  indexText <- c(indexText,"```",timestamp(),"```")
  writeLines(indexText, paste(directory, "/markdown/index.md", sep = ""))
  knit2html(
    paste(directory, "/markdown/index.md", sep = ""),
    paste(directory, "/static/index.md", sep = ""),
    stylesheet = style,
    title=pageTitle
  )
}

#Add a "back" link to each page.
addBack <- function(fileInfo){
  headerPath <- paste(directory, "/markdown/header.md", sep = "")
  header <- readChar(headerPath, file.info(headerPath)$size)
  content <- fileInfo$content
  content <- paste(header, content, collapse = "")
  fileInfo$content <- content
  fileInfo
}

#Create an HTML Page from Markdown File
makeHtml <- function(fileInfo,style,directory) {
  message("Making",fileInfo$title)
  fileInfo <- addBack(fileInfo)
  tempFile <- paste(directory, "/temp/temp.md", sep = "")
  writeLines(fileInfo$content, tempFile, sep = "")
  knit2html(tempFile, output=fileInfo$fullStaticPath, stylesheet = style)
  setwd(directory)
}


#Remove Priority String
removePriority <- function(content){
  content <- str_remove(content,"[+][0-9]{1,10}")
  content
}


#Parse the files and make a dataframe with relevant file info.
makeFileData <- function(directory){
  fileName=c()
  folder=c()
  fullMarkdownPath=c()
  fullStaticPath=c()
  title=c()
  content=c()
  priority <- c()
  folders <- getFolders(directory)
  for (fileFolder in folders) {
    files <- getFiles(fileFolder, directory)
    for(file in files){
      fileMarkdownPath <- getMarkdownPath(file,fileFolder,directory)
      fileStaticPath <- getStaticPath(file,fileFolder,directory)
      fileTitle <- getTitle(fileMarkdownPath)
      filePriority <- as.numeric(getPriority(fileMarkdownPath))
      fileContent <- readChar(fileMarkdownPath, file.info(fileMarkdownPath)$size)
      fileContent <- removePriority(fileContent)
      priority <- c(priority,filePriority)
      fileName <- c(fileName,file)
      folder <- c(folder,fileFolder)
      fullMarkdownPath <- c(fullMarkdownPath,fileMarkdownPath)
      fullStaticPath <- c(fullStaticPath,fileStaticPath)
      title <- c(title,fileTitle)
      content <- c(content,fileContent)
    }
  }
  return(data.frame(fileName = fileName,folder = folder,fullMarkdownPath = fullMarkdownPath,fullStaticPath = fullStaticPath,title = title,priority = priority,content = content,stringsAsFactors=FALSE))
}


#Make sure the directory structure of the site is ok.
setupSite <- function(directory){
  setwd(directory)
  dir.create(paste(c(directory, "/static/"), collapse = ""))
  folders <- getFolders(directory)
  setupTemp(directory)
  setupStatic(directory, folders)
}


#Put it All Together to Make Page
makePage <- function(directory, style,pageTitle,indexHeader) {
  print("Setting Up Site")
  setupSite(directory)
  print("Getting File Data")
  fileData <- makeFileData(directory)
  for(i in 1:dim(fileData)[1]){
    print("Setting up Page")
    print(fileData[i,]$title)
    makeHtml(fileData[i,],style = style,directory=directory)
  }
  print("Making Index")
  makeIndex(fileData,pageTitle)
}

#Set it up. Make it go.
#directory = dirname(sys.frame(1)$ofile)
#setwd(directory)

# Local Building
directory = getwd()


style = paste(c(directory,"/style/style.css"),collapse="")
makePage(directory,style,"Greg Leo")


```

## Style Sheet

```{r,style,eval=FALSE,echo=TRUE}
@import url('https://fonts.googleapis.com/css2?family=Share+Tech+Mono&display=swap');

:root {
  --bgcol: #312F2f;
	--header1bgcol: #BF98A0;
	--header2bgcol: #BF98A0;
	--header1fgcol: #312F2f;
	--header2fgcol: #312F2f;
  --textcol: #a6D9F7;
	--linkcol: #84DCCF;
	--visitedlinkcol: #84DCCF;
	--subheadercol: #BF98A0;
	--shadowcol: #312F2f;
	--pretextcol: #84DCCF;
	--prebgcol: #312F2f;
	--operator: #BF98A0;
	--string: #84DCCF;
	--identifier: #a6D9F7;
	--number: #a6D9F7;
	--keyword: #a6D9F7;
	--literal: #a6D9F7;
	--comment: #BF98A0;
}

body {
	background: var(--bgcol);
	font-family: "Roboto Mono", monospace;
	font-weight: 500;
	color: var(--textcol);
	-webkit-font-smoothing: antialiased;
	font-size: 16pt;
	width: 900px;
	line-height: 18pt;
	padding-left: 5pt;
	padding-right: 5pt;
	padding-top: 5pt;
	margin: auto;
}

main h1 {}

h1 {
	font-family: "Rubik Mono One", sans-serif;
	background: var(--header1bgcol);
	color: var(--header1fgcol);
	font-size: 24pt;
	text-transform: uppercase;
	display: inline-block;
	margin-top: 15px;
	margin-bottom: 10px;
	font-weight: bold;
	line-height: 26pt;
	padding: 2px 2px;
}

h2 {
	font-family: "Rubik Mono One", sans-serif;
	background: var(--header2bgcol);
	color: var(--header2fgcol);
	font-size: 20pt;
	text-transform: uppercase;
	margin-top: 15px;
	margin-bottom: 10px;
	font-weight: bold;
	line-height: 22pt;
	padding: 2px 2px;
}

h3 {
	margin-top: 16px;
	margin-bottom: 5px;
}

h4 {
	margin-top: 16px;
	margin-bottom: 5px;
}

title {
	font-weight: bold;
	line-height: 26pt;
}

p.subheader {
	font-weight: bold;
	color: var(--subheadercol);
}

img {
	padding: 3pt;
	float: center;
}

a {
	color: var(--linkcol);
	color: inherit;
	text-shadow: 1px 1px var(--shadowcol), -1px -1px var(--shadowcol);
	text-decoration: underline;
	font-size: 16pt;
}

a:link,
a:visited {
	color: var(--visitedlinkcol);
}

pre {
	overflow-x: auto;
	white-space: pre-wrap;
	white-space: -moz-pre-wrap;
	white-space: -pre-wrap;
	white-space: -o-pre-wrap;
	word-wrap: break-word;
	padding: 2px 5px;
	color: var(--pretextcol);
	background: var(--prebgcol);
	font-family: inherit;
}

a:hover,
a:active {
	text-decoration: none;
	/*text-decoration: line-through;*/
}

p {
	margin: 0px;
	margin-bottom: 5px;

}

div.footer {
	font-size: 9pt;
	font-style: italic;
	line-height: 12pt;
	text-align: center;
	padding-top: 30pt;
}


pre .operator, pre .paren {
    color: var(--operator);
}

pre .string, pre .paren {
    color: var(--string);
}

pre .identifier, pre .paren {
    color: var(--identifier);
}

pre .number, pre .paren {
    color: var(--number);
}

pre .keyword, pre .paren {
    color: var(--number);
}

pre .literal, pre .paren {
    color: var(--number);
}

pre .constant, pre .paren {
    color: var(--number);
}
```
