import java.io.File;
import java.util.Arrays;

import java.util.ArrayList;

public class TagsJar {
  private ArrayList<File> _jars = new ArrayList<>(); // absolute paths of jar files
  private String _tagFilename;
  private Tags _tags;

  public TagsJar(String[] args) {
    if (args.length < 2) {
      printHelpMessage();
      System.exit(0);
    }
    _tags = new Tags(args[0]);
    setJars(Arrays.copyOfRange(args, 1, args.length));
  }

  private void printHelpMessage() {
    System.out.println("Usage: java TagsJar [tagfilename] [jarfile]...");
  }

  private void run() {
    if (_jars.size() > 0) {
      _tags.processJarFiles(_jars.toArray(new File[0]));
    }
  }

  public ArrayList<File> getJars() {
    return _jars;
  }

  public void setJars(String[] jars) {
    // add existent jar files
    for (String jar : jars) {
      File f = new File(jar);
      if (jar.endsWith(".jar") && f.exists()) {
        _jars.add(f.getAbsoluteFile());
      }
      else {
        System.err.println(String.format("Warning: jar %s cannot be found, so passed", jar));
      }
    }
  }

  public static void main(String[] args) {
    new TagsJar(args).run();
  }
}
