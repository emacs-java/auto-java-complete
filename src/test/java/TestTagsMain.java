import java.io.ByteArrayOutputStream;

import java.io.ByteArrayInputStream;

import java.io.StringReader;

import java.io.InputStream;

import org.junit.Test;
import org.junit.After;
import org.junit.Before;

import static org.junit.Assert.*;

public class TestTagsMain {
  private TagsMain _tagsMain;

  @Before
  public void setUp() {
    _tagsMain = new TagsMain();
  }

  @Test
  public void testReadYesOrNo() {
    InputStream in = new ByteArrayInputStream("y".getBytes());
    boolean ret = _tagsMain.readYesOrNo(new ByteArrayOutputStream(), in);
    assertTrue("Shoud be true", ret);
    in = new ByteArrayInputStream("n".getBytes());
    ret = _tagsMain.readYesOrNo(new ByteArrayOutputStream(), in);
    org.junit.Assert.assertFalse(ret);
  }
}
