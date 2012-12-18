import java.io.File;
import java.util.regex.Pattern;

public class TagsMain {
  public static void main(String[] argv) throws Exception {
    System.out.println(
      "*****************************************************************\n" +
      "**   this program will need about 3 to 15 min,                  **\n" +
      "**   maybe half an hour ,(just kidding),but you must be patient.**\n" +
      "**   before it exit,you may see a few exceptions                **\n" +
      "**   If it don't kill the program ,just ignore it .             **\n" +
      "**   and when you add a jar to classpath, you'd better make sure**\n" +
      "**   all jars it depends on are in classpath.                   **\n" +
      "******************************************************************\n"
      );

    System.out.println("log file is located at: " +
                       System.getProperty("java.io.tmpdir") + "/ajc_error.log");
    System.out.println("log file is located at: " +
                       System.getProperty("java.io.tmpdir") +
                       "/ajc_info.log\n\n");

    System.out.println(
      "********************************************************************************************\n" +
      "***    you can use this Class like this:                                                 ***\n" +
      "***           java Tags                                                                  ***\n" +
      "***    all class  in classpath will be tagged.                                           ***\n" +
      "***                                                                                      ***\n" +
      "***           java Tags \"org\\.hello,org\\.world\"                                      ***\n" +
      "***    it would NOT tag those class match \"org.hello\" or \"org.world\" .               ***\n" +
      "***                                                                                      ***\n" +
      "***           java -cp yourclasspath Tags                                                ***\n" +
      "***  if you see java.lang.OutOfMemoryError: PermGen space ,you can increment permsize:   ***\n" +
      "***       java -XX:MaxPermSize=512m -Xms256M -Xmx512M Tags                               ***\n" +
      "***                                                                                      ***\n" +
      "***  before that you'd better backup the  file ~/.java_base.tag,if exists                ***\n" +
      "*******************************************************************************************\n\n"
      );
    try {
      System.out.println("sleep 20 seconds...");
      // for debug purpose, set it to 3 sec
      Thread.sleep(3000);
    } catch (Exception ex) {}

    Tags tags = new Tags();
    if (argv.length > 0) {
      String[] regexs = argv[0].split(",");
      tags._classExcludeRegexPatternArray = new Pattern[regexs.length];
      for (int m = 0; m < tags._classExcludeRegexPatternArray.length; m++) {
        tags._classExcludeRegexPatternArray[m] = Pattern.compile(regexs[m].replaceAll("\"" , "").replaceAll("'" , ""));
      }
    }
    tags.process() ;

    System.out.println(
      "\n******************************************************************************************\n" +
      "***                  exit successful!!!                                                  ***\n" +
      "***you will see a file named '.java_base.tag' in your home directory                     ***\n" +
      "***  the size of the generated ~/.java_base.tag  is about 2M or bigger,so if your        ***\n" +
      "*** .java_base.tag  is too small ,that means your CLASSPATH don't configure correctly.   ***\n" +
      "********************************************************************************************\n"
      );
    System.out.println(new File(tags.getHomePath(), ".java_base.tag").getAbsolutePath());
    System.exit(0);
  }
}
