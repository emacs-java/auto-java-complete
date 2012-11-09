import static org.junit.Assert.*;

import java.util.Vector;

import java.lang.reflect.Method;

import org.junit.Test;

public class TagsTest {

  @Test
  public void testTagClass() {
    try {
      Method target = Tags.class.getDeclaredMethod("tagClass", java.lang.Class.class);
      target.setAccessible(true);
      //ClassItem clsItem = (ClassItem)target.invoke(new Tags(), org.junit.Test.class);
      ClassItem clsItem = (ClassItem)target.invoke(new Tags(), java.util.Vector.class);
      assertEquals("Vector", clsItem.name);
      assertEquals("java.util", clsItem.pkgItem.name);
    } catch (Exception e) {
      assertTrue(e.getCause() instanceof ApplicationException);
    }
  }
}
