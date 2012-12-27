// -*- coding: utf-8-unix; -*-
import java.io.File;
import java.util.regex.Pattern;

public class TagsMain {
  // for debug purpose, set it to 3 sec
  private static final int SLEEP_TIME = 3000;

  private Tags _tags = new Tags();

  public static void main(String[] args) throws Exception {
    new TagsMain().run(args);
    System.exit(0);
  }

  public void run(String[] args) throws Exception {
    printStartMessage();
    try {
      System.out.println(String.format("sleep %s seconds...", SLEEP_TIME));
      Thread.sleep(SLEEP_TIME);
    } catch (Exception ex) {}

    if (args.length > 0) {
      String[] regexs = args[0].split(",");
      _tags.setClassExcludeRegexPatternArray(new Pattern[regexs.length]);
      for (int m = 0; m < _tags.getClassExcludeRegexPatternArray().length; m++) {
        _tags.setClassExcludeRegexPatternArray(
          m, Pattern.compile(regexs[m].replaceAll("\"", "").replaceAll("'", "")));
      }
    }
    _tags.process();
    printEndMessage();
  }

  private void printStartMessage() {
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
  }

  private void printEndMessage() {
    System.out.println(
      "\n******************************************************************************************\n" +
      "***                  exit successful!!!                                                  ***\n" +
      "***you will see a file named '.java_base.tag' in your home directory                     ***\n" +
      "***  the size of the generated ~/.java_base.tag  is about 2M or bigger,so if your        ***\n" +
      "*** .java_base.tag  is too small ,that means your CLASSPATH don't configure correctly.   ***\n" +
      "********************************************************************************************\n"
      );
    System.out.println(new File(_tags.getHomePath(), ".java_base.tag").getAbsolutePath());
  }
}
