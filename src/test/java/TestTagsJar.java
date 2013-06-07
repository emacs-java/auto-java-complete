import java.io.File;
import java.util.ArrayList;

import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class TestTagsJar {
  // wanna write this on build.xml...
  private String _someclassJar = "../../../test/someclass.jar";

  @Test
  public void testTagsJar() {
    File jarfile = new File(_someclassJar);
    String[] args = { "sometagname.tag", jarfile.getAbsolutePath(), };
    TagsJar tagsJar = new TagsJar(args);
    ArrayList<File> jarlist = tagsJar.getJars();
    assertEquals("should be 1", 1, jarlist.size());
    assertEquals("jar filename should be someclass.jar",
                 jarfile.getAbsolutePath(), jarlist.get(0).getAbsolutePath());
  }
}
