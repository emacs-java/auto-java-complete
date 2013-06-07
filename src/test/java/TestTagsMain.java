import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.StringReader;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class TestTagsMain {
  private TagsMain _tagsMain;

  @Before
  public void setUp() {
    _tagsMain = new TagsMain();
  }

  @Test
  public void testReadYesOrNo() {
    InputStream in = new ByteArrayInputStream(
      String.format("y%s", System.getProperty("line.separator")).getBytes());
    boolean ret = _tagsMain.readYesOrNo(new ByteArrayOutputStream(), in);
    assertTrue("Shoud be true", ret);
    in = new ByteArrayInputStream(String.format("n%s",
                                                System.getProperty("line.separator")).getBytes());
    ret = _tagsMain.readYesOrNo(new ByteArrayOutputStream(), in);
    org.junit.Assert.assertFalse("Shoud be false", ret);
  }
}
