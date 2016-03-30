/**
 * Runs when the add-on is installed.
 *
 * @param {object} e The event parameter for a simple onInstall trigger. To
 *     determine which authorization mode (ScriptApp.AuthMode) the trigger is
 *     running in, inspect e.authMode. (In practice, onInstall triggers always
 *     run in AuthMode.FULL, but onOpen triggers may be AuthMode.LIMITED or
 *     AuthMode.NONE.)
 */
function onInstall(e) {
  onOpen(e);
}


/**
 * Creates a menu entry in the Google Docs UI when the document is opened.
 *
 * @param {object} e The event parameter for a simple onOpen trigger. To
 *     determine which authorization mode (ScriptApp.AuthMode) the trigger is
 *     running in, inspect e.authMode.
 */
function onOpen(e) {
  DocumentApp.getUi().createAddonMenu()
      .addItem('Start New Manuscript','startNewManuscript')
      .addItem('Insert Data', 'showSidebar')
      .addItem('Review Data','showReviewBar')
      .addToUi();
  var ui = DocumentApp.getUi();
  Logger.log(e.authMode);
}

function startNewManuscript(){
  checkUntitledFileName();
  checkAuxFiles();  
}

/*
* BOTTOM BAR
*/

function showReviewBar(){
  var ui = HtmlService.createHtmlOutputFromFile('ReviewBar')
           .setTitle('Review Data Requests')
           .setSandboxMode(HtmlService.SandboxMode.IFRAME);
  DocumentApp.getUi().showSidebar(ui);
}

/**
 * Opens a sidebar in the document containing the add-on's user interface.
 */
function showSidebar() {
  var ui = HtmlService.createHtmlOutputFromFile('Sidebar')
      .setTitle('Data Requests')
      .setSandboxMode(HtmlService.SandboxMode.IFRAME);
  DocumentApp.getUi().showSidebar(ui);
}


/**
 * Get spreadsheet Data
 */
function getDataInjections(){
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  var data = sheet.getDataRange().getValues();
  return data;
}

/**
* get Spreadsheet Row By Key
*/

function getSpreadsheetRowByKey(key){
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  var data = sheet.getDataRange().getValues();
  for(i=0;i<data.length;i++){
    if(data[i][0] == key){
      return data[i]; 
    }
  }
}


/**
 * Inject spreadsheet Data
 */
function injectSpreadsheetData(){
  Logger.log("injectSpreadSheetData called");
  /* Get spreadsheet data and the active doc. */
  var data = getDataInjections();
  var body = DocumentApp.getActiveDocument().getBody();
  
  /* 
  + Loop through rows of spreadsheet data
  + Search the text of the document for keystring {key:n wildcard } 
  + Replace the wildcard with the value in the spreadsheet
  + Color the new keystring
  
  NOTES: Coloring add a bit of code.  Without coloring, replacement
         could be done with: body.replaceText(searchString, replaceString);
  */
  for(var i=0; i<data.length; i++){
    var searchString = "\\{key:"+data[i][0]+" (.*?)\\}";
    var replaceString = "\{key:"+data[i][0]+" "+data[i][1]+"\}";
    var replaceString2 ="{key:"+data[i][0]+" "+data[i][1]+"}";
    var searchResult = body.findText(searchString);
    if(searchResult !== null){
      var searchElement = searchResult.getElement();
      var searchElementText = searchElement.asText();
      searchElementText.replaceText(searchString,replaceString).asText();
      var replacedRangeElement = searchElementText.findText(escapeRegExp(replaceString2));
      searchElementText.setForegroundColor(replacedRangeElement.getStartOffset(), replacedRangeElement.getEndOffsetInclusive(),'#fb8644');
      var key = "{key:"+data[i][0];
      searchElementText.setForegroundColor(replacedRangeElement.getStartOffset(), replacedRangeElement.getStartOffset() + key.length ,'#b1b1b1');
      searchElementText.setForegroundColor(replacedRangeElement.getEndOffsetInclusive(), replacedRangeElement.getEndOffsetInclusive() ,'#b1b1b1');
    }
  }
  return "Data successfully injected";
}


/*
** CHECK FOR UNTITLED FILE NAME
*/
function  checkUntitledFileName(){
  var name = DocumentApp.getActiveDocument().getName();
  if(name==="Untitled document"){
    var q1 = DocumentApp.getUi();
    var r1 = q1.prompt('Please name this document:');
    var new_name = r1.getResponseText();
    DocumentApp.getActiveDocument().setName(new_name);
  }
}


/*
** CHECK FOR AUX FILES - IF NONEXISTANT, CREATE
*/
function checkAuxFiles(){
  var docProperties = PropertiesService.getDocumentProperties();
  var data_injections_id = docProperties.getProperty('data-injections-id');
  var name = DocumentApp.getActiveDocument().getName();
  var parent_folder = getMostRecentlyUpdatedParentFolder();
  
  Logger.log("data-injections-id: "+data_injections_id);
  
  if(data_injections_id === null){
    var data_injections = {
      "title": name+"-data-injections",
      "mimeType": "application/vnd.google-apps.spreadsheet",
      "parents": [{"id": parent_folder.getId()}]
    };
   
    var data_injections_file = Drive.Files.insert(data_injections);
    docProperties.setProperty('data-injections-id',data_injections_file.id);
    Logger.log(data_injections_file.id);
  
    //Add first row to spreadsheet
    var last_key_value = getLastKeyValue();
    var docProperties = PropertiesService.getDocumentProperties();
    var fileId = docProperties.getProperty('data-injections-id');
    var sheet = SpreadsheetApp.openById(fileId);
    sheet.appendRow([(Number(last_key_value)).toString()," "," "," ","#Add preliminary code here"]);
    iterateKeyValue(1);
  }
}

/*
** CLEAR AUX FILES
*/

function clearAuxFiles(){
  // Delete all user properties in the current script.
  var docProperties = PropertiesService.getDocumentProperties();
  docProperties.deleteProperty('data-injections-id');
  docProperties.deleteProperty('last-key-vaule');
  //userProperties.deleteAllProperties();
}


/*
*  GET THE LAST USED KEY VALUE;  If non-existant, sets to 0;
*/
function getLastKeyValue(){
  var docProperties = PropertiesService.getDocumentProperties();
  var last_key_value = docProperties.getProperty('last-key-value');
  if(last_key_value === null){
    docProperties.setProperty('last-key-value',0);
    last_key_value = 0;
  }
  return last_key_value;
}


/*
*  ITERATE KEY VALUE BY NUM;
*/
function iterateKeyValue(num){
  var docProperties = PropertiesService.getDocumentProperties();
  var last_key_value = docProperties.getProperty('last-key-value');
  docProperties.setProperty('last-key-value',(Number(last_key_value) + num).toString());
}


/**
*  REQUEST IN-LINE DATA
*/

function requestInlineData(){
  var htmlOutput = HtmlService
     .createHtmlOutputFromFile('RequestInlineData')
     .setSandboxMode(HtmlService.SandboxMode.IFRAME)
     .setWidth(300)
     .setHeight(300);
  DocumentApp.getUi().showModalDialog(htmlOutput, 'Request In-line Data');
}


/**
*  REQUEST FIGURE
*/

function requestFigure(){
  var htmlOutput = HtmlService
     .createHtmlOutputFromFile('RequestFigure')
     .setSandboxMode(HtmlService.SandboxMode.IFRAME)
     .setWidth(300)
     .setHeight(300);
  DocumentApp.getUi().showModalDialog(htmlOutput, 'Request Figure');
}

/**
*  REQUEST TABLE
*/

function requestTable(){
  var htmlOutput = HtmlService
     .createHtmlOutputFromFile('RequestTable')
     .setSandboxMode(HtmlService.SandboxMode.IFRAME)
     .setWidth(300)
     .setHeight(300);
  DocumentApp.getUi().showModalDialog(htmlOutput, 'Request Table');
}

/**
*   Add requested in-line data to data injections spreadsheet and add key to document;
*/

function addRequestedInlineDataToSpreadsheet(format,instructions){
  var last_key_value = getLastKeyValue();  
  insertText("{key:"+last_key_value+" "+format+"}")
  
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  sheet.appendRow([last_key_value,format,format,instructions]);
  iterateKeyValue(1);  
}


/**
*   Add requested figure to data injections spreadsheet and add key to document;
*/

function addRequestedFigureToSpreadsheet(instructions){
  var last_key_value = getLastKeyValue();  
  insertText("{key:"+last_key_value+" "+instructions+"}")
  
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  sheet.appendRow([last_key_value,"figure","figure",instructions]);
  iterateKeyValue(1);  
}

/**
*   Add requested table to data injections spreadsheet and add key to document;
*/

function addRequestedTableToSpreadsheet(instructions){
  var last_key_value = getLastKeyValue();  
  insertText("{key:"+last_key_value+" "+instructions+"}")
  
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  sheet.appendRow([last_key_value,"table","table",instructions]);
  iterateKeyValue(1);  
}



/**
*   Add requested table to data injections spreadsheet and add key to document;
*/

function addRequestedTableToSpreadsheet(instructions){
  var last_key_value = getLastKeyValue();  
  insertText("{key:"+last_key_value+" "+instructions+"}")
  
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  sheet.appendRow([last_key_value,"table","table",instructions]);
  iterateKeyValue(1);  
}

/**
* Replace line of spreadsheet data by key
*/

function replaceLineOfSpreadsheetDataByKey(line){
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  var data = sheet.getDataRange();
  var data_table = data.getValues();
  for(i=0;i<data_table.length;i++){
    if(data_table[i][0] == line.key){
      data_table[i][1] = line.calculated_value;
      data_table[i][2] = line.format;
      data_table[i][3] = line.instructions;
      data_table[i][4] = line.code;
    }
  }
  data.setValues(data_table); 
}


/**
*  Insert text at cursor
*/

function insertTextAtCursor (text){
 // Insert some text at the cursor position and make it bold.
 var cursor = DocumentApp.getActiveDocument().getCursor();
 if (cursor) {
   // Attempt to insert text at the cursor position. If the insertion returns null, the cursor's
   // containing element doesn't allow insertions, so show the user an error message.
   var element = cursor.insertText(text);
   if (element) {
     element.setForegroundColor("#fb4444");
   } else {
     DocumentApp.getUi().alert('Cannot insert text here.');
   }
 } else {
   DocumentApp.getUi().alert('Cannot find a cursor.');
 }
}



/*
*  GET TEXT OF SELECTION
*/

/**
 * Gets the text the user has selected. If there is no selection,
 * this function displays an error message.
 *
 * @return {Array.<string>} The selected text.
 */
function getTextOfSelection() {
  var selection = DocumentApp.getActiveDocument().getSelection();
  if (selection) {
    var text = [];
    var elements = selection.getSelectedElements();
    for (var i = 0; i < elements.length; i++) {
      if (elements[i].isPartial()) {
        var element = elements[i].getElement().asText();
        var startIndex = elements[i].getStartOffset();
        var endIndex = elements[i].getEndOffsetInclusive();

        text.push(element.getText().substring(startIndex, endIndex + 1));
      } else {
        var element = elements[i].getElement();
        // Only translate elements that can be edited as text; skip images and
        // other non-text elements.
        if (element.editAsText) {
          var elementText = element.asText().getText();
          // This check is necessary to exclude images, which return a blank
          // text element.
          if (elementText != '') {
            text.push(elementText);
          }
        }
      }
    }
    if (text.length == 0) {
      throw 'Please select some text.';
    }
    return text;
  } else {
    throw 'Please select some text.';
  }
}

// THE CODE BELOW WAS MODIFIED FROM GOOGLE DOCS DOCUMENTATION.  THE CODE ABOVE WAS COPY AND PASTED FROM
// THE GOOGLE START-UP GUIDE
//
//function getTextOfSelection(){
//  var selection = DocumentApp.getActiveDocument().getSelection();
//  var output = "";
//  if (selection) {
//    var elements = selection.getRangeElements();
//    for (var i = 0; i < elements.length; i++) {
//      var element = elements[i];
//      
//      // Only modify elements that can be edited as text; skip images and other non-text elements.
//      if (element.getElement().editAsText) {
//        var text = element.getElement().editAsText().getText();
//        
//        // Get selected part of the element, or the full element if it's completely selected.
//        if (element.isPartial()) {
//          output = output + " " + text.substring(element.getStartOffset(), element.getEndOffsetInclusive()+1);
//        } else {
//          output = output + " " + text;
//        }
//      }
//    }
//  }
//  Logger.log(output);
//  return output;
//}

/** FROM GOOGLE START UP GUIDE
 * Replaces the text of the current selection with the provided text, or
 * inserts text at the current cursor location. (There will always be either
 * a selection or a cursor.) If multiple elements are selected, only inserts the
 * translated text in the first element that can contain text and removes the
 * other elements.
 *
 * @param {string} newText The text with which to replace the current selection.
 */
function insertText(newText) {
  var style = {};
  style[DocumentApp.Attribute.FOREGROUND_COLOR] = "#fb4444";

  var selection = DocumentApp.getActiveDocument().getSelection();
  if (selection) {
    var replaced = false;
    var elements = selection.getSelectedElements();
    if (elements.length == 1 &&
        elements[0].getElement().getType() ==
        DocumentApp.ElementType.INLINE_IMAGE) {
      throw "Can't insert text into an image.";
    }
    for (var i = 0; i < elements.length; i++) {
      if (elements[i].isPartial()) {
        var element = elements[i].getElement().asText();
        var startIndex = elements[i].getStartOffset();
        var endIndex = elements[i].getEndOffsetInclusive();

        var remainingText = element.getText().substring(endIndex + 1);
        element.deleteText(startIndex, endIndex);
        if (!replaced) {
          element.insertText(startIndex, newText);
          element.editAsText().setAttributes(startIndex,startIndex + newText.length - 1,style);
          replaced = true;
        } else {
          // This block handles a selection that ends with a partial element. We
          // want to copy this partial text to the previous element so we don't
          // have a line-break before the last partial.
          var parent = element.getParent();
          parent.getPreviousSibling().asText().appendText(remainingText);
          // We cannot remove the last paragraph of a doc. If this is the case,
          // just remove the text within the last paragraph instead.
          if (parent.getNextSibling()) {
            parent.removeFromParent();
          } else {
            element.removeFromParent();
          }
        }
      } else {
        var element = elements[i].getElement();
        if (!replaced && element.editAsText) {
          // Only translate elements that can be edited as text, removing other
          // elements.
          element.clear();
          element.asText().setText(newText);
          replaced = true;
        } else {
          // We cannot remove the last paragraph of a doc. If this is the case,
          // just clear the element.
          if (element.getNextSibling()) {
            element.removeFromParent();
          } else {
            element.clear();
          }
        }
      }
    }
  } else {
    var cursor = DocumentApp.getActiveDocument().getCursor();
    var surroundingText = cursor.getSurroundingText().getText();
    var surroundingTextOffset = cursor.getSurroundingTextOffset();

    // If the cursor follows or preceds a non-space character, insert a space
    // between the character and the translation. Otherwise, just insert the
    // translation.
    if (surroundingTextOffset > 0) {
      if (surroundingText.charAt(surroundingTextOffset - 1) != ' ') {
        newText = ' ' + newText;
      }
    }
    if (surroundingTextOffset < surroundingText.length) {
      if (surroundingText.charAt(surroundingTextOffset) != ' ') {
        newText += ' ';
      }
    }
    cursor.insertText(newText).setAttributes(style);
  }
}




// From answer http://stackoverflow.com/a/9102463/1677912
function getFirstEmptyRowByColumnArray(sheet) {
  var column = sheet.getRange('A:A');
  var values = column.getValues(); // get all data in one call
  var ct = 0;
  while ( values[ct] && values[ct][0] != "" ) {
    ct++;
  }
  return (ct+1);
}

/**
** Escape user input for use in regEx searches.
**/

function escapeRegExp(string){
  return string.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}


/*
*  Get key list
*/ 

function getKeyValues(){
  var theBody = DocumentApp.getActiveDocument().getBody();
  var searchString = "\\{key:[0-9]* ";
  var searchResult = theBody.findText(searchString);
  var idx = 0;
  var keyValues = [];
  
  while (searchResult != null) {
    key = searchResult.getElement().asText().getText().substring(searchResult.getStartOffset()+5,searchResult.getEndOffsetInclusive());
    keyValues[idx] = key;
    idx = idx + 1;
    
    // Find the next match
    var searchResult = theBody.findText(searchString,searchResult);
  }
  return keyValues;
}

/*
*  Generate R markdown document from spreadsheet
*/
function generateRmarkdownDoc(){
  var docProperties = PropertiesService.getDocumentProperties();
  var fileId = docProperties.getProperty('data-injections-id');
  var sheet = SpreadsheetApp.openById(fileId);
  var data = sheet.getDataRange().getValues();
  var todays_date = new Date().toISOString().substring(0,9);
  var fileName =  DocumentApp.getActiveDocument().getName() ;
  var Rmd = '---\ntitle: Data Elements for ' + fileName + '\ndate: ' + todays_date + '\noutput: html_document\n---\n\n```{r}\nrequire(googlesheets)\n# If using googleSheets for first time, look at\n# https://github.com/jennybc/googlesheets/blob/master/vignettes/managing-auth-tokens.Rmd\n# for instructions on saving the Google authorization token.\ngs_auth(token = "~/gst.rds")\ndocObject<-gs_key("' + fileId + '",visibility="private")\ndf <- gs_read(docObject, header=FALSE)\n```\n\n';  
  var theKeyValues = getKeyValues();
  for(i=0;i<data.length;i++){
    var iplusone = parseInt(i) + 1;
    if(data[i][0] in theKeyValues){
      var newentry = '### Key: ' + data[i][0] + '\nInstructions:\n' + data[i][3] + '\n\nFormat: \n' + data[i][2] + '\n```{r key:' + data[i][0] + '}\n' + data[i][4] + '\n\n\n# assign output to key'+i+'\nif(exists("key'+i+'")) gs_edit_cells(docObject, input=key'+ i +', anchor="B'+iplusone+'")\n```\n\n---\n\n';
      Rmd = Rmd + newentry;
    }
  }
  var parent_folder = getMostRecentlyUpdatedParentFolder();
  var stat_code = parent_folder.createFile(fileName + "-stat-code.Rmd",Rmd,MimeType.PLAIN_TEXT);  
}


/*
* Play
*/

function play(){
  var lkv = getLastKeyValue();
  Logger.log(lkv);
  iterateKeyValue(1);
  lkv = getLastKeyValue();
  Logger.log(lkv);
  
// var body = DocumentApp.getActiveDocument().getBody();
//  
// // Create a two-dimensional array containing the cell contents.
// var cells = [
//   ['Data-Injections', 'ABCawer9082ypr4bnrewlfkjbhnasrotfi78q40rt9hgfju'],
//   ['Code', 'BCq1ihf0794yt02thrjhwe340298th3498tO']
// ];
//
// // Build a table from the array.
// var np = body.appendParagraph("Location of linked data (DO NOT DELETE --");
// var info_table = body.appendTable(cells);
//  
//  // Define a custom paragraph style.
// var style = {};
// style[DocumentApp.Attribute.FONT_SIZE] = 8;
// style[DocumentApp.Attribute.BOLD] = false;
// style[DocumentApp.Attribute.FOREGROUND_COLOR] = "#b1b1b1";
// info_table.setAttributes(style).setBorderColor("#b1b1b1");
// np.setAttributes(style);

}



function addCustomProperty(key,value) {
  var file_id = DocumentApp.getActiveDocument().getId();
  var property = {
    key: key,
    value: value,
    visibility: 'PUBLIC'
  };
  Drive.Properties.insert(property, file_id);
}


function doSomething() {
  Logger.log('I was called!');
}


// OLD STUFF (PROBABLY CAN BE DELETED)
/**
* Get ID of spreadsheet.  Chooses the most recently updated one if there are multiple;
**/
function getNewestSpreadsheetByName(name){
  var files = DriveApp.getFilesByName(name);
  var date = new Date(2000,1,1);
  while (files.hasNext()) {
    var file = files.next();
    if(file.getMimeType() === "application/vnd.google-apps.spreadsheet" && file.getLastUpdated().valueOf() >= date.valueOf()){
      var id = file.getId();
      var date = file.getLastUpdated();
    }
  }
  return id;
}


/**
* Get ID of spreadsheet IN PARENT FOLDER.  Chooses the most recently updated one if there are multiple;
**/
function getNewestSpreadsheetInParentFolderByName(name){
  var file_id = DocumentApp.getActiveDocument().getId();
  var file = DriveApp.getFileById(file_id);
  var folder_iterator = file.getParents();
  
  var files = DriveApp.getFilesByName(name);
  var date = new Date(2000,1,1);
  while (files.hasNext()) {
    var file = files.next();
    if(file.getMimeType() === "application/vnd.google-apps.spreadsheet" && file.getLastUpdated().valueOf() >= date.valueOf()){
      var id = file.getId();
      var date = file.getLastUpdated();
    }
  }
  return id;
}


/**
 * seeTheDoc
**/

function seeTheDoc(){
 var thisDoc = DocumentApp.getActiveDocument();
 var bblob = thisDoc.getBody().getAttributes();
 Logger.log(bblob);
}


/*
*
* AS THE NAME SAYS: returns the most recently updated parent folder
*/

function getMostRecentlyUpdatedParentFolder(){
  var file_id = DocumentApp.getActiveDocument().getId();
  var file = DriveApp.getFileById(file_id);
  var folder_iterator = file.getParents();
  var date = new Date(2000,1,1);
  while (folder_iterator.hasNext()) {
    var folder = folder_iterator.next();
    if(folder.getLastUpdated().valueOf() > date.valueOf()){
      date = folder.getLastUpdated();
      parent_folder = folder;
    }
  }
  Logger.log(parent_folder.getName());
  return parent_folder;
}

