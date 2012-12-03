import static org.junit.Assert.*;

import org.junit.Test;
import org.junit.runners.Parameterized.Parameters;
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

      // clsItem = (ClassItem)target.invoke(new Tags() , org.junit.runners.Parameterized.Parameters.class);
      // assertEquals("Parameters", clsItem.name);
      // System.out.println(clsItem.pkgItem.name.toString());
      //assertEquals("org.junit.runners.Parameterized", clsItem.pkgItem.name);

      Class cls = org.junit.runners.Parameterized.Parameters.class;
      System.out.println("class name: " + cls.getName());
      System.out.println("package name: " + cls.getName().substring(0, cls.getName().lastIndexOf('$')));
      System.out.println("simple name: " + cls.getSimpleName());
      System.out.println("package name: " + cls.getPackage().getName());
      System.out.println("isAnnotation: " + cls.isAnnotation());
      System.out.println("isAnonymousClass: " + cls.isAnonymousClass());
      clsItem = (ClassItem)target.invoke(new Tags() , org.junit.runners.Parameterized.Parameters.class);
      System.out.println("");
      System.out.println("clsItem.name: " + clsItem.name);
      System.out.println("clsIitem.pkgItem.name: " + clsItem.pkgItem.name);
    } catch (Exception e) {
      System.out.println(e.getCause());
      fail("This can't be!");
    }
  }
}
