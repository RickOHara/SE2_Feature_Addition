package pms;

import java.awt.Desktop;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.mime.MultipartEntity;
import org.apache.http.entity.mime.content.FileBody;
import org.apache.http.entity.mime.content.StringBody;
import org.apache.http.impl.client.DefaultHttpClient;

/**
 *
 * @author Thomas Lindley
 */
public class Uploader 
{
    private String title;
//    private String image;
    private File image;
    
    
    String IMGUR_URI = "https://api.imgur.com/3/upload.json";
    String CLIENT_ID = "54ff02afbdba867";
    String IMGUR_API_KEY = "09e92c36b548cb6878a62a9437b629abdae58c3c";

    Uploader(String t, File im)
    {
        title = t;
        image = im;
    }
    
    String Upload()
    {
        //create post
        HttpPost post = new HttpPost(IMGUR_URI);
        
        //add authorization to the post header
        post.addHeader("Authorization", "Client-ID " + CLIENT_ID);
        
        MultipartEntity entity = new MultipartEntity();
        HttpClient client = new DefaultHttpClient();
        HttpResponse response;
        String JSONresponse = null;
        
        try 
        {
            //add content to entity
            entity.addPart("key", new StringBody(IMGUR_API_KEY));
            entity.addPart("image", new FileBody(image));
            entity.addPart("title", new StringBody(this.title));

            //set the entity for the post
            post.setEntity(entity);
            response = client.execute(post);

            //create streams for writing out the response
            InputStream content = response.getEntity().getContent();
            ByteArrayOutputStream responseStream = new ByteArrayOutputStream();

            //initialize to -1 in case of failure
            int mybyte = -1;
            
            //loop through content and write content to output stream
            while((mybyte = content.read()) > -1)
            {
                responseStream.write(mybyte);
            }
            content.close();
            
            JSONresponse = responseStream.toString();
        }
        
        //catch exceptions
        catch (IOException e) 
        {
            e.printStackTrace();
        } 
        
        return JSONresponse;
    }
    
    void openLink(String link)
    {
        //open the link if Desktop is supported
        if(Desktop.isDesktopSupported())
        {
            try 
            {
                Desktop.getDesktop().browse(new URI(link));
            } 
            catch (IOException e) 
            {
                e.printStackTrace();
            } 
            catch (URISyntaxException e) 
            {
                e.printStackTrace();
            }
        }
    }
}