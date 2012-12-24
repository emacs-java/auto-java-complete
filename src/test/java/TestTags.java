import static org.junit.Assert.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class TestTags {
  @Test
  public void testTagClassOk() throws ApplicationException {
    Tags tags = new Tags();
    ClassItem cItem = tags.tagClass(java.util.Vector.class);
    assertEquals(java.util.Vector.class, cItem.getClazz());
    assertEquals("Vector", cItem.getName());
    assertEquals("package name should be java.util", "java.util", cItem.getPackageName());
    assertTrue(tags.getPackages().contains(cItem.getPackageItem()));
  }
}
