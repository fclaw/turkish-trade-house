import * as https from 'https'; // or 'https' for https:// URLs
import * as fs from 'fs';

var url = process.argv.slice(2)[0];

console.log('swagger host: ' + url);

const file = fs.createWriteStream("api.json");
https.get(url, function(response) {
   response.pipe(file);

   // after download completed close file stream
   file.on("finish", () => {
       file.close();
       console.log("Download Completed");
   });
});