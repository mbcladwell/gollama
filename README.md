

<h2>Local</h2>

1. Convert document to plain text paragraphs; impose minimum size on the paragraphs
   (gollama ollama)  (ingest-doc doc id model uri top-dir) 
                                     
									 


<h2>Remote</h2>

2. Calculate embeddings for each paragraph; store as json in <top-dir>/db/
    (gollama ollama)   (recurse-process-para id para counter plst elst model uri top-dir) gets embeddings

3. Calculate embedding for query


4. Calculate similarity of query embedding with each paragraph; sort

5. Extract top N most similar paragraphs

6. Append to system prompt and submit


<h2>File naming conventions</h2>

<h3> <top-dir>/text directory</h3>

MyFile.txt  original ascii text file

<h3> <top-dir>/db directory</h3>

|suffix|File name|Description|
|-----|-----|----|

|npar|acbf1a82b78d-npar.json  |original file with small paragraphs (< 20 tokens) combined into normalized paragraphs|
|ipar|acbf1a82b78d-ipar.json |acbf1a82b78d-npar.json now with indices in json format.|
|embe|acbf1a82b78d-embe.json |embeddings for associated ipar.json file matched by index|

|qtxt|acbf1a82b78d-qtxt.json |indexed plain text queries|
|qemb|acbf1a82b78d-qemb.json |indexed embeddings for queries|

|qsys|acbf1a82b78d-qsys.json |indexed plain text system prompts|
|qesy|acbf1a82b78d-qesy.json |indexed embeddings for system prompts|


<h2>db.json</h2>

[{"id":"acbf1a82b78d",
"doc":"/home/mbc/projects/gollama/text/ppan.txt",
"title":"ppan",
"model":"mistral:latest",
"algorithm":"cosine-sim",
"date":"241213092503"}]
