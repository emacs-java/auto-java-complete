import static org.junit.Assert.*;
import org.junit.Test;
import java.util.Vector;
import java.lang.reflect.Method;

public class TagsTest {

  @Test
  public void testTagClass() {
    try {
      Method target = Tags.class.getDeclaredMethod("tagClass", java.lang.Class.class);
      target.setAccessible(true);
      ClassItem clsItem = (ClassItem)target.invoke(new Tags(), org.junit.Test.class);
      assertEquals("Test", clsItem.name);
      assertEquals("org.junit", clsItem.pkgItem.name);
    } catch (Exception e) {
      System.out.println(e.getCause());
      fail("This can't be!");
    }
  }
}
