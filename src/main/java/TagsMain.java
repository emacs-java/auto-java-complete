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
      System.out.println(String.format("sleep %s millseconds...", SLEEP_TIME));
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
      "******************************************************************\n" +
      "***   This program will need about 3 to 15 min,                ***\n" +
      "***   so you you have to be patient                            ***\n" +
      "***   Before exiting, you may see a few exceptions.            ***\n" +
      "***   If it don't kill the program, just ignore it.            ***\n" +
      "***   When you add a jar to classpath, you'd better make sure  ***\n" +
      "***   all jars it depends on are in classpath.                 ***\n" +
      "******************************************************************\n"
      );

    System.out.println("Log file is located at: " +
                       System.getProperty("java.io.tmpdir") + "/ajc_error.log");
    System.out.println("Log file is located at: " +
                       System.getProperty("java.io.tmpdir") +
                       "/ajc_info.log\n\n");

    System.out.println(
      "******************************************************************\n" +
      "***    You can use this Class like this:                       ***\n" +
      "***           java Tags                                        ***\n" +
      "***    all classes in classpath will be tagged.                ***\n" +
      "***                                                            ***\n" +
      "***           java Tags \"org\\.hello,org\\.world\"            ***\n" +
      "***    This would NOT tag those classes that match             ***\n" +
      "***    \"org.hello\" or \"org.world\".                         ***\n" +
      "***                                                            ***\n" +
      "***           java -cp yourclasspath Tags                      ***\n" +
      "***  If you see java.lang.OutOfMemoryError: PermGen space,     ***\n" +
      "***  you need to increase permsize:                            ***\n" +
      "***       java -XX:MaxPermSize=512m -Xms256M -Xmx512M Tags     ***\n" +
      "***                                                            ***\n" +
      "***  Before that, you'd better backup the file                 ***\n" +
      "***   ~/.java_base.tag if exists                               ***\n" +
      "******************************************************************\n\n"
      );
  }

  private void printEndMessage() {
    System.out.println(
      "\n****************************************************************\n" +
      "***                  Exit successful!!!                        ***\n" +
      "***  You will see a file named '.java_base.tag'                ***\n" +
      "***  in your home directory.                                   ***\n" +
      "***  The size of the generated ~/.java_base.tag is             ***\n" +
      "***  about 2M or bigger, so if your .java_base.tag is          ***\n" +
      "***  too small, that mabey means your CLASSPATH isn't          ***\n" +
      "***  configureed properly.                                     ***\n" +
      "******************************************************************\n"
      );
    System.out.println(new File(_tags.getHomePath(), ".java_base.tag").getAbsolutePath());
  }
}
