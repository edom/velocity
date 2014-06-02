import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Properties;
import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;

public class Main
{
    public static void main (String[] args) throws IOException
    {
        final String templateName = args[0];
        final Properties p = new Properties();
        final VelocityEngine ve = new VelocityEngine();
        p.setProperty(RuntimeConstants.RESOURCE_LOADER, "file");
        p.setProperty(RuntimeConstants.FILE_RESOURCE_LOADER_PATH, "../test");
        ve.init(p);
        final Template t = ve.getTemplate(templateName);
        final VelocityContext context = new VelocityContext();
        final Writer writer = new OutputStreamWriter(System.out);
        t.merge(context, writer);
        writer.flush();
    }
}
