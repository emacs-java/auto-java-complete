// -*- coding: utf-8-unix; -*-
// Tags - make a tags table by reflection

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.


// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// So here’s how to generate the home directory’s tags file:
//     java -cp $JAVA_HOME/jre/lib/rt.jar Tags
//     java -cp /path/to/your/jars_and_classesfiles/:$JAVA_HOME/jre/lib/rt.jar Tags
//  or java Tags
//  or java Tags "javax\." ,exclude javax.*.ClassName
//  or java Tags "javax\.,java\." ,exclude javax.**ClassName,and java.**ClassName
// then it will generate a file ~/.java_base.tag in your home directory

//Sometimes you may see some exceptions like:
//
//java.lang.ClassNotFoundException: org.apache.log4j.pattern.BridgePatternParser
//
//that means this class   is not in your classpath or some imported class in
//org.apache.log4j.pattern.BridgePatternParser is not in your classpath,
//you must find out all jars it depends on ,and put it in your classpath.
//or if you think this class is not very important ,you can just ignore this
//exception, it will not be indexed.
//maybe this link may do help to you :http://www.findjar.com

import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.regex.*;
import java.util.jar.*;
import java.util.zip.*;


/**
 * Make a tags table from the class files in a classpath.
 * The classpath is obtained from the property <code>java.class.path</code>
 *
 * The class names that get output must match the <code>classExcludeRegexPatternArray</code>
 * if it is specified.
 *
 * @author joseph <jixiuf@gmail.com>
 */
public class Tags {

  private static final int SHIFT = 6;

  private BufferedWriter _tagFile = null;
  private PrintWriter _logError = null;
  private PrintWriter _logInfo = null;
  private List<Class> _clss = new LinkedList<Class>();
  private List<PackageItem> _packages = new LinkedList<PackageItem>();
  private List<ClassItem> _classes = new LinkedList<ClassItem>();
  private List<MemberItem> _members = new LinkedList<MemberItem>();
  private String _fileSeparator = System.getProperty("file.separator");
  private Pattern[] _classExcludeRegexPatternArray = null;
  private File _randomTmpPath = new File(System.getProperty("java.io.tmpdir") +
                                         File.separatorChar +
                                         UUID.randomUUID().toString() +
                                         File.separatorChar);
  private ClassLoader _cl = new CL(_randomTmpPath);

  public Pattern[] getClassExcludeRegexPatternArray() { return _classExcludeRegexPatternArray; }
  public void setClassExcludeRegexPatternArray(Pattern[] patternArray) {
    _classExcludeRegexPatternArray = patternArray;
  }
  public void setClassExcludeRegexPatternArray(int m, Pattern pattern) {
    _classExcludeRegexPatternArray[m] = pattern;
  }

  public Tags() {
    try {
      _tagFile = new BufferedWriter(new FileWriter(new File(getHomePath(), ".java_base.tag")));
      _logError = new PrintWriter(new File(System.getProperty("java.io.tmpdir"), "ajc_error.log"));
      _logInfo = new PrintWriter(new File(System.getProperty("java.io.tmpdir"), "ajc_info.log"));
    } catch (Exception e) {
      System.err.print(e.getMessage());
    }
  }

  public Tags(String tagFilename) {
    try {
      _tagFile = new BufferedWriter(new FileWriter(new File(tagFilename).getAbsolutePath()));
      _logError = new PrintWriter(new File(System.getProperty("java.io.tmpdir"), "ajc_error.log"));
      _logInfo = new PrintWriter(new File(System.getProperty("java.io.tmpdir"), "ajc_info.log"));
    } catch (Exception e) {
      System.err.print(e.getMessage());
    }
  }

  public String getHomePath() {
    String home = System.getenv("HOME");
    if (home == null) {
      home = System.getProperty("user.home");
    }
    return home;
  }

  private void log(Throwable e) {
    System.err.println("cause:" + e.getCause() + "  " + e.getClass().getName() + ":" + e.getMessage());
    e.printStackTrace(_logError);
  }

  private void logInfo(String info) {
    _logInfo.println(info);
  }

  /** If this is not null it's used as a filter for acceptable classes.
   * Only packages that <code>matches(_classExcludeRegexPatternArray)</code> will be tagged.
   */

  //copy $CLASSPATH/**.class to _randomTmpPath
  //unzip $CLASSPATH/**.jar to _randomTmpPath
  //CLASSLOADER CL will load class from this directory.
  private void prepare() {
    String classpath = System.getProperty("java.class.path");
    String [] cls = classpath.split(System.getProperty("path.separator"));
    for (int i = 0; i < cls.length; i++) {
      String element = cls[i];
      if (element.equals(".") || element.equals("./") || element.equals(".\\")) {
        continue;
      }
      File f = new File(element);
      if (f.exists()) {
        if (f.isDirectory()) {
          processDirectory(f);
        } else {
          processJarFile(f);
        }
      }
    }
  }

  //clean _randomTmpPath directory
  protected void clear() {
    if (_randomTmpPath != null && _randomTmpPath.isDirectory()) {
      IOUtils.del(_randomTmpPath);
      _randomTmpPath.delete();
    }
  }

  // Entry point and called from TagsMain
  public void process() {
    prepare();//copy or unzip *.class *.jar to _randomTmpPath
    if (_randomTmpPath == null || _randomTmpPath.isDirectory() == false) {
      return;
    }
    List<File> clazzFiles = collectClassFiles();
    processClasses(clazzFiles);
    tagAll();
    write();
    clear();
  }

  // Another entry point, called from TagsJar
  public void processJarFiles(File[] files) {
    if (_randomTmpPath == null || _randomTmpPath.isDirectory() == false) {
      System.err.println("Warning: random tmp directory cannot be found.");
      return;
    }
    for (File file : files) {
      processJarFile(file);
    }
    List<File> clazzFiles = collectClassFiles();
    processClasses(clazzFiles);
    tagAll();
    write();
    clear();
  }

  private List<File> collectClassFiles() {
    List<File> clazzFiles = null;
    if (_randomTmpPath == null || _randomTmpPath.isDirectory() == false) {
      return clazzFiles;
    }
    System.out.println("tmp classpath :" + _randomTmpPath.getAbsolutePath());
    logInfo("tmp classpath :" + _randomTmpPath.getAbsolutePath());
    clazzFiles = IOUtils.getAllFilesUnderDir(_randomTmpPath,
                                             new FileFilter() {
                                               public boolean accept(File f) {
                                                 if (f.getName().endsWith(".class")) { return true; }
                                                 return false;
                                               }
                                             });
    return clazzFiles;
  }

  public void processClasses(List<File> clazzFiles) {
    for (File clazz : clazzFiles) {
      String classAbsolutePath = clazz.getAbsolutePath();
      String classFullName = classAbsolutePath
        .substring(_randomTmpPath.getAbsolutePath().length() + 1,
                   classAbsolutePath.indexOf(".class"))
        .replace(_fileSeparator, ".");
      processClass(classFullName);
    }
  }

  private void processJarFile(File f) {
    Unzip.unzip(f, _randomTmpPath);
    System.out.println("adding " + f.getAbsolutePath() + " to classpath...");
    logInfo("adding " + f.getAbsolutePath() + " to classpath...");
  }

  /**
   * Collect Class objects represented by className to be tagged.
     @param className  full className
  */
  private void processClass(String className) {
    if (className.startsWith("sun")) { return; }
    if (className.startsWith("com.sun")) { return; }
    if (className.startsWith("com.thaiopensource")) { return; }
    if (className .contains("org.iso_relax.ant")) { return; }
    if (_classExcludeRegexPatternArray != null) {
      for (int i = 0; i < _classExcludeRegexPatternArray.length; i++) {
        if (_classExcludeRegexPatternArray[i].matcher(className).find()) {
          return;
        }
      }
    }
    try {
      // Class c = Class.forName(className,false,ClassLoader.getSystemClassLoader());
      Class c = Class.forName(className, false, _cl);
      _clss.add(c);
    } catch (Throwable t) {
      log(t);
    }
  }

  /** Find class file and jar in the directory.
   *  and process the found class file and jar file with processJar,and processClass
   */
  private void processDirectory(File dir) {
    if (dir != null && dir.isDirectory()) {
      String dirFullPath = dir.getAbsolutePath();
      List<File> clazzFiles = IOUtils.getAllFilesUnderDir
        (dir, new FileFilter() {
            public boolean accept(File f) {
              if (f.getName().endsWith(".class")) { return true; }
              return false;
            }
          });
      for (File clazz : clazzFiles) {
        String classAbsolutePath = clazz.getAbsolutePath();
        IOUtils.copy(clazz, new File(_randomTmpPath, classAbsolutePath.substring(dirFullPath.length() + 1)));
      }
      List<File> jarz = IOUtils.getAllFilesUnderDir(dir,
                                                    new FileFilter() {
                                                      public boolean accept(File f) {
                                                        if (f.getName().endsWith(".jar")) {
                                                          return true;
                                                        }
                                                        return false;
                                                      }
                                                    });
      if (jarz != null) {
        for (File jarFile: jarz) {
          processJarFile(jarFile);
        }
      }
    }
  }

  // Process each class in _clss
  private void tagAll() {
    System.out.println("found " + _clss.size() + " classes.");
    logInfo("found " + _clss.size() + "  classes.");
    try {
      for (Class c : _clss) {
        try {
          ClassItem cItem = tagClass(c);
          _classes.add(cItem);
        } catch (ApplicationException e) {
          logInfo(e.getMessage());
        } catch (Throwable ex) {
          log(ex);
        }
      }

      Collections.sort(_packages);
      Collections.sort(_classes);
      System.out.println("tagged " + _classes.size() + " classes.");
      logInfo("tagged " + _classes.size() + " classes.");

      for (ClassItem cItem : _classes) {
        try {
          List<MemberItem> localMems = tagConstructors(cItem);
          localMems.addAll(tagMethods(cItem));
          localMems.addAll(tagFields(cItem));
          cItem.setMembers(localMems);
        } catch (ApplicationException e) {
          logInfo(e.getMessage());
        } catch (Throwable ex) {
          log(ex);
        }
      }
      int pkg_size = _packages.size();
      int classes_size = _classes.size();
      PackageItem pkgItem = null;
      ClassItem cItem = null;

      for (int i = 0; i < pkg_size; i++) {
        // now _packages are sorted, so the line num of packages in
        // tag file is the index+1 in _packages list
        _packages.get(i).setLineNum(SHIFT + i + 1);
      }
      for (int i = 0; i < classes_size; i++) {
        // the line number of class in tag file is the count of packages
        // plus the index of the class in classes list
        // in this loop, we will populte the lineNum of each ClassItem and
        // populate the classStartLineNum and classEndLineNum of each package
        cItem = _classes.get(i);
        cItem.setLineNum(SHIFT + pkg_size + i + 1);
        if (i == 0) {
          pkgItem = cItem.getPackageItem();
          pkgItem.setClassStartLineNum(cItem.getLineNum());
        } else if (pkgItem != cItem.getPackageItem()) {
          pkgItem.setClassEndLineNum(cItem.getLineNum());
          pkgItem = cItem.getPackageItem();
          pkgItem.setClassStartLineNum(cItem.getLineNum());
        }
      }
      if (cItem != null && pkgItem != null) {
        //TODO: cItem maybe null here ,bugfix
        pkgItem.setClassEndLineNum(cItem.getLineNum() + 1); //populate the last pkgLast
      }
      pkgItem = null;
      cItem = null;

      for (int i = 0; i < classes_size; i++) {
        // in this loop ,we will populate basic info about each member of class into  (exclude)
        cItem = _classes.get(i);
        cItem.setMemStartLineNum(SHIFT + pkg_size + classes_size + _members.size() + 1);
        if (cItem.getMembers() != null) {
          _members.addAll(cItem.getMembers());
          cItem.setMemEndLineNum(cItem.getMemStartLineNum() + cItem.getMembers().size() - 1);
          cItem.setMembers(null);
        }
      }

      cItem = null;

      MemberItem memItem = null;
      int members_size = _members.size();
      int memberLineNum_start = SHIFT + pkg_size + classes_size + 1;
      for (int i = 0; i < members_size; i++) {
        memItem = _members.get(i);
        memItem.setLineNum(memberLineNum_start + i);
        if (i == 0) {
          cItem = memItem.getClassItem();
          cItem.setMemStartLineNum(memItem.getLineNum());
        } else if (cItem != memItem.getClassItem()) {
          cItem.setMemEndLineNum(memItem.getLineNum());
          cItem = memItem.getClassItem();
          cItem.setMemEndLineNum(memItem.getLineNum());
        }
      }
      if (cItem != null && memItem != null) {
        cItem.setMemEndLineNum(memItem.getLineNum() + 1);
      }
      memItem = null;
      cItem = null;
    } catch (Throwable t) {
      log(t);
    }
  }

  /**
   * Check if this Class c is to be excluded or not.
   */
  private void checkClassToExclude(Class c) throws ApplicationException {
    if (c.isAnonymousClass()) {
      throw new ApplicationException("sorry, you are an AnnonymousClass:" + c.getName());
    }
    if (c.isArray()) {
      throw new ApplicationException("sorry, you are an Array:" + c.getName());
    }
    if (c.isPrimitive()) {
      throw new ApplicationException("sorry, you are a Primitive type:" + c.getName());
    }
    if (c.getSimpleName().equals("")) {
      throw new ApplicationException("why don't you have a name. I don't know how to handle you:" +
                                     c.getName());
    }
    if ((!Modifier.isPublic(c.getModifiers())) &&
        (!c.isInterface()) &&
        (!Modifier.isAbstract(c.getModifiers()))) {
      throw new ApplicationException("sorry, you are not a public class:" + c.getName());
    }
    if (c.getPackage() == null) {
      throw new ApplicationException("why don't you hava a package name?:" + c.getName());
    }
  }

  // analyze Class c, and populate PackageItem with its package info,
  // and populate ClassItem with its class info,
  // then add them to _classes and _packages list
  protected ClassItem tagClass(Class c) throws ApplicationException {
    checkClassToExclude(c);
    String pkgName = c.getPackage().getName();
    if (c.isAnnotation() && c.getName().contains("$")) {
      pkgName = c.getName().substring(0 , c.getName().lastIndexOf('$'));
    }
    PackageItem pkgItem = null;
    // check if pkgName is in list.
    for (int i = 0; i < _packages.size(); i++) {
      if (_packages.get(i).getName().equals(pkgName)) {
        pkgItem = _packages.get(i);
        break;
      }
    }
    // if there's no such pkgItem, we create a new one and insert it
    // into list.
    if (pkgItem == null) {
      pkgItem = new PackageItem(pkgName);
      _packages.add(pkgItem);
    }
    // then we create a new ClassItem and return it.
    ClassItem cItem = new ClassItem(c, c.getSimpleName(), pkgItem);
    for (ClassItem ci : _classes) {
      if (ci.equals(cItem)) {
        throw new ApplicationException("you have already in, why come here again! :" + c.getName());
      }
    }
    return cItem;
  }

  // maybe there are bugs here, i think i should write it depend on
  // different type, like annotation enum and so on
  private ClassItemWrapper getClassItemWrapper(Class clazz) {
    ClassItemWrapper returnType = new ClassItemWrapper();
    if (clazz.isPrimitive()) {
      returnType.setAlternativeString(clazz.getName());
    } else if (clazz.isArray()) {
      returnType.setAlternativeString(clazz.getName());
      String typeName = clazz.getName();
      if (typeName.contains("[I")) {
        String tmp = "int";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[F")) {
        String tmp = "float";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[Z")) {
        String tmp = "boolean";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[J")) {
        String tmp = "long";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[B")) {
        String tmp = "byte";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[C")) {
        String tmp = "char";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[S")) {
        String tmp = "char";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[D")) {
        String tmp = "";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType.setAlternativeString(tmp);
      } else if (typeName.contains("[L")) {
        int index = (typeName.indexOf("[L"));
        String className = typeName.substring(index + 2, typeName.length() - 1);
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { className += "[]"; }
        }
        returnType.setAlternativeString(className);
      }
    } else if (clazz.isAnnotation()) {
      returnType.setAlternativeString(clazz.getName());
      // do nothing
    } else if (clazz.isEnum()) {
      returnType.setAlternativeString(clazz.getName());
    } else {
      for (ClassItem ci : _classes) {
        if (clazz.getName() != null && clazz.getName().equals(ci.getCls().getName())) {
          returnType.setClassItem(ci);
          break;
        }
      }
      if (returnType.getClassItem() == null) {
        returnType.setAlternativeString(clazz.getName());
      }
    }
    return returnType;
  }

  // tag Field
  // Extract public fields and create and return a list
  // containing info of those fields.
  protected List<MemberItem> tagFields(ClassItem cItem) throws Throwable {
    Field[] fields = cItem.getCls().getDeclaredFields();
    List<MemberItem> localMems = new ArrayList<MemberItem>();
    for (int i = 0; i < fields.length; i++) {
      if (!Modifier.isPublic(fields[i].getModifiers())) {
        continue;
      }
      Class fieldType = (Class)fields[i].getType();
      MemberItem memItem = new MemberItem(fields[i], fields[i].getName(), cItem);
      memItem.setReturnType(getClassItemWrapper(fieldType));
      localMems.add(memItem);
    }
    Collections.sort(localMems);
    return localMems;
  }

  protected List<MemberItem> tagConstructors(ClassItem cItem) throws Throwable {
    Constructor[] methods = cItem.getCls().getDeclaredConstructors();
    List<MemberItem> localMems = new ArrayList<MemberItem>();
    for (int i = 0; i < methods.length; i++) {
      if (!Modifier.isPublic(methods[i].getModifiers())) { continue; }
      String name = methods[i].getName();
      if (name.contains(".")) {
        name = name.substring(name.lastIndexOf(".") + 1);
      }
      MemberItem memItem = new MemberItem(methods[i], name, cItem);
      Class[] params = methods[i].getParameterTypes();
      List<ClassItemWrapper> paramsKV = new ArrayList<ClassItemWrapper>();
      for (Class param : params) {
        paramsKV.add(getClassItemWrapper(param));
      }
      memItem.setParams(paramsKV);

      Class[] exceptions = methods[i].getExceptionTypes();
      List<ClassItemWrapper> exceptionsKV = new ArrayList<ClassItemWrapper>();
      for (Class e : exceptions) {
        exceptionsKV.add(getClassItemWrapper(e));
      }
      memItem.setExceptions(exceptionsKV);
      localMems.add(memItem);
    }
    Collections.sort(localMems);
    return localMems;
  }

  protected List<MemberItem> tagMethods(ClassItem cItem) throws Throwable {
    // Method[] methods = cItem.cls.getDeclaredMethods();
    Method[] methods = cItem.getCls().getMethods();
    List<MemberItem> localMems = new ArrayList<MemberItem>();
    for (int i = 0; i < methods.length; i++) {
      if (!Modifier.isPublic(methods[i].getModifiers())) { continue; }
      MemberItem memItem = new MemberItem(methods[i], methods[i].getName(), cItem);
      memItem.setReturnType(getClassItemWrapper(methods[i].getReturnType()));
      Class[] params = methods[i].getParameterTypes();
      List<ClassItemWrapper> paramsKV = new ArrayList<ClassItemWrapper>();
      for (Class param: params) { paramsKV.add(getClassItemWrapper(param)); }
      memItem.setParams(paramsKV);

      Class[] exceptions = methods[i].getExceptionTypes();
      List<ClassItemWrapper> exceptionsKV = new ArrayList<ClassItemWrapper>();
      for (Class e: exceptions) { exceptionsKV.add(getClassItemWrapper(e)); }
      memItem.setExceptions(exceptionsKV);
      localMems.add(memItem);
    }
    Collections.sort(localMems);
    return localMems;
  }

  private void writeTagHeader(BufferedWriter writer) throws Exception {
    writer.append("don't try to edit this file ,even this line!!!!");
    writer.newLine();
    writer.append("package count=" + _packages.size() +
                    "  ,Class count=" + _classes.size() +
                    " , member count(constructor, field, method)= " + _members.size());
    writer.newLine();
    writer.append("" + (SHIFT + 1));
    writer.newLine();
    writer.append("" + (SHIFT + _packages.size() + 1));
    writer.newLine();
    writer.append("" + (SHIFT + _packages.size() + _classes.size() + 1));
    writer.newLine();
    writer.append("" + (SHIFT + _packages.size() + _classes.size() + _members.size() + 1));
    writer.newLine();
  }

  private <T> void writeItemInfo(BufferedWriter writer, List<T> items) throws Exception {
    int i = 0;
    for (T item : items) {
      writer.append(item.toString());
      writer.newLine();
      if (i % 300 == 0) { writer.flush(); }
      i++;
    }
    writer.flush();
  }

  private void write() {
    try {
      writeTagHeader(_tagFile);
      writeItemInfo(_tagFile, _packages);
      writeItemInfo(_tagFile, _classes);
      writeItemInfo(_tagFile, _members);
    } catch (Exception e) {
      System.err.println(e.getMessage());
    } finally {
      try {
        _tagFile.close();
        _tagFile = null;
      } catch (Exception e) {
        e.printStackTrace();
      }
      try {
        _logError.close();
        _logError = null;
      } catch (Exception e) {
        e.printStackTrace();
      }
      try {
        _logInfo.close();
        _logInfo = null;
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
  }

  public List<PackageItem> getPackages() { return _packages; }
}

class PackageItem implements Comparable<PackageItem> {
  private String _name;
  private int _lineNum;
  private int _classStartLineNum;
  private int _classEndLineNum;
  private Package _pkg;

  public PackageItem(String name) {
    _name = name;
  }

  public int compareTo(PackageItem pkgItem) {
    return _name.compareTo(pkgItem._name);
  }

  public String toString() {
    return _name + "`" + _classStartLineNum + "`" + _classEndLineNum;
  }

  public String getName() { return _name; }
  public int getLineNum() { return _lineNum; }

  public void setLineNum(int lineNum) { _lineNum = lineNum; }
  public void setClassStartLineNum(int classStartLineNum) {
    _classStartLineNum = classStartLineNum;
  }
  public void setClassEndLineNum(int classEndLineNum) { _classEndLineNum = classEndLineNum; }
}

class ClassItem implements Comparable<ClassItem> {
  private String _name;
  private int _lineNum;
  private int _memStartLineNum;
  private int _memEndLineNum;
  private Class _cls;
  private PackageItem _pkgItem;
  private List<MemberItem> _members;

  public void setMembers(List<MemberItem> members) { _members = members; }
  public void setMemStartLineNum(int memStartLineNum) { _memStartLineNum = memStartLineNum; }
  public void setMemEndLineNum(int memEndLineNum) { _memEndLineNum = memEndLineNum; }
  public void setLineNum(int lineNum)  { _lineNum = lineNum; }

  public int getLineNum() { return _lineNum; }
  public int getMemStartLineNum() { return _memStartLineNum; }
  public int getMemEndLineNum() { return _memEndLineNum; }
  public List<MemberItem> getMembers() { return _members; }

  public ClassItem(Class cls, String name, PackageItem pkgItem) {
    _cls = cls;
    _name = name;
    _pkgItem = pkgItem;
  }

  public ClassItem(Class cls) {
    _cls = cls;
    _name = cls.getSimpleName();
    _pkgItem = new PackageItem(cls.getPackage().getName());
  }

  public int compareTo(ClassItem cItem) {
    int pkgCmp = _pkgItem.getName().compareTo(cItem._pkgItem.getName());
    if (pkgCmp != 0) { return pkgCmp; }
    return (_name.compareTo(cItem._name));
  }

  public int hashCode() {
    if (_pkgItem == null || _pkgItem.getName() == null) { return _name.hashCode(); }
    return (_name + "." + _pkgItem.getName()).hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof ClassItem)) { return false; }
    ClassItem other = (ClassItem)obj;
    if (_pkgItem == null && other._pkgItem == null && _name != null) {
      return _name.equals(other._name);
    }
    if (_pkgItem != null && other._pkgItem != null && _name != null &&
        _name.equals(other._name) && _pkgItem.getName() != null &&
        _pkgItem.getName().equals(other._pkgItem.getName())) {
      return true;
    }
    return false;
  }

  public String toString() {
    return _name + "`" + _pkgItem.getLineNum() + "`" + _memStartLineNum + "`" + _memEndLineNum;
  }

  public PackageItem getPackageItem() { return _pkgItem; }
  public String getPackageName() { return _pkgItem.getName(); }
  public String getName() { return _name; }
  public Class getCls() { return _cls; }
}

class MemberItem implements Comparable<MemberItem> {
  private String _name;
  private ClassItem _cItem;
  private int _lineNum;
  private List<ClassItemWrapper> _params;     // used for constructors and methods
  private List<ClassItemWrapper> _exceptions; // used for constructors and methods
  private ClassItemWrapper _returnType;       // typename of fields, return value
  private Field _field;
  private Method _method;
  private Constructor _constructor;

  public MemberItem(Method method, String name, ClassItem cItem) {
    _method = method;
    _name = name;
    _cItem = cItem;
  }

  public MemberItem(Constructor constructor, String name, ClassItem cItem) {
    _constructor = constructor;
    _name = name;
    _cItem = cItem;
  }

  public MemberItem(Field field, String name, ClassItem cItem) {
    _field = field;
    _name = name;
    _cItem = cItem;
  }

  public void setParams(List<ClassItemWrapper> params) { _params = params; }
  public void setExceptions(List<ClassItemWrapper> exceptions) { _exceptions = exceptions; }
  public void setReturnType(ClassItemWrapper returnType) { _returnType = returnType; }
  public void setLineNum(int lineNum) { _lineNum = lineNum; }

  public int getLineNum() { return _lineNum; }
  public ClassItem getClassItem() { return _cItem; }
  public String getName() { return _name; }
  public List<ClassItemWrapper> getParams() { return _params; }
  public Method getMethod() { return _method; }
  public ClassItemWrapper getReturnType() { return _returnType; }

  public String toString() {
    StringBuffer returnStr = new StringBuffer();
    if (_constructor != null) {
      returnStr.append("  " + _name + "`");
      // append params
      if (_params != null) {
        for (ClassItemWrapper param: _params) {
          if (param.getAlternativeString() != null) {
            returnStr.append("~" + param.getAlternativeString() + ",");
          } else {
            returnStr.append(param.getClassItem().getLineNum() + ",");
          }
        }
        if (_params.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
      returnStr.append("`");
      //append exceptions
      if (_exceptions != null) {
        for (ClassItemWrapper exp : _exceptions) {
          if (exp.getAlternativeString() != null) {
            returnStr.append("~" + exp.getAlternativeString() + ",");
          } else {
            returnStr.append(exp.getClassItem().getLineNum() + ",");
          }
        }
        if (_exceptions.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
    } else if (_field != null) {
      returnStr.append(" " + _name + "`");
      //returnStr.append(cItem.lineNum+"`");
      // apend the field type
      if (_returnType.getAlternativeString() != null) {
        returnStr.append("~" + _returnType.getAlternativeString());
      } else {
        returnStr.append(_returnType.getClassItem().getLineNum());
      }
    } else if (_method != null) {
      returnStr.append(_name + "`");
      // append returnType
      if (_returnType.getAlternativeString() != null) {
        returnStr.append("~" + _returnType.getAlternativeString());
      } else {
        if (_returnType.getClassItem() == null) {
          System.out.println("mem.name=" + _name);
          System.out.println(_method.getDeclaringClass().getName());
        }
        returnStr.append(_returnType.getClassItem().getLineNum());
      }
      returnStr.append("`");
      // append params
      if (_params != null) {
        for (ClassItemWrapper param : _params) {
          if (param.getAlternativeString() != null) {
            returnStr.append("~" + param.getAlternativeString() + ",");
          } else {
            returnStr.append(param.getClassItem().getLineNum() + ",");
          }
        }
        if (_params.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
      returnStr.append("`");
      // append exceptions
      if (_exceptions != null) {
        for (ClassItemWrapper exp : _exceptions) {
          if (exp.getAlternativeString() != null) {
            returnStr.append("~" + exp.getAlternativeString() + ",");
          } else {
            returnStr.append(exp.getClassItem().getLineNum() + ",");
          }
        }
        if (_exceptions.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
    }
    return returnStr.toString();
  }

  public int compareTo(MemberItem memItem) {
    int classCompareResult = _cItem.compareTo(memItem._cItem);
    if (classCompareResult != 0) {
      return classCompareResult;
    }
    if (_field != null) {
      if (memItem._field == null) { return -1; }
      return _name.compareTo(memItem._name);
    } else if (_constructor != null) {
      if (memItem._constructor == null) { return -1; }
      int cmp = _name.compareTo(memItem._name);
      if (cmp != 0) { return cmp; }
      return _constructor.toString().compareTo(memItem._constructor.toString());
    } else if (_method != null) {
      if (memItem._method == null) { return -1; }
      int cmp = _name.compareTo(memItem._name);
      if (cmp != 0) { return cmp; }
      return _method.toString().compareTo(memItem._method.toString());
    } else {
      return toString().compareTo(memItem.toString());
    }
  }
}

class ClassItemWrapper {
  private ClassItem _cItem;
  private String _alternativeString;

  public ClassItem getClassItem() { return _cItem; }
  public String getAlternativeString() { return _alternativeString; }

  public void setClassItem(ClassItem cItem) { _cItem = cItem; }
  public void setAlternativeString(String alternativeString ) {
    _alternativeString = alternativeString;
  }
}

class ApplicationException extends Exception {
  public ApplicationException(String msg) {
    super(msg);
  }
}

class Unzip {
  /** unzip zip file to sDestPath
   * @param sZipPathFile :path of zip file
   * @param sDestPath    :path ,where to put the ectracted files
   *
   */
  public static void unzip(File sZipPathFile, File sDestPath) {
    try {
      FileInputStream fins = new FileInputStream(sZipPathFile);
      ZipInputStream zins = new ZipInputStream(fins);
      ZipEntry ze = null;
      byte ch[] = new byte[256];
      while ((ze = zins.getNextEntry()) != null) {
        File zfile = new File(sDestPath , ze.getName());
        File fpath = new File(zfile.getParentFile().getPath());
        if (ze.isDirectory()) {
          if (!zfile.exists()) {
            zfile.mkdirs();
          }
          zins.closeEntry();
        } else {
          if (!fpath.exists()) {
            fpath.mkdirs();
          }
          FileOutputStream fouts = new FileOutputStream(zfile);
          int i;
          while ((i = zins.read(ch)) != -1) {
            fouts.write(ch, 0, i);
          }
          zins.closeEntry();
          fouts.close();
        }
      }
      fins.close();
      zins.close();
    } catch (Exception e) {
      System.err.println("Extract error(maybe bad zip(jar) file.):" + e.getMessage());
    }
  }
}

class CL extends ClassLoader {

  private File _classBasePath = null;

  public CL(File classBasePath) {
    _classBasePath = classBasePath;
    if (!_classBasePath.exists()) {
      _classBasePath.mkdirs();
    }
  }

  public Class<?> findClass(String name) throws ClassNotFoundException {
    File classFile = new File(_classBasePath, name.replace(".", File.separator) + ".class");
    if (!classFile.exists()) { throw new ClassNotFoundException("Can't find class:" + name); }
    // Add the package information
    final int packageIndex = name.lastIndexOf('.');
    if (packageIndex != -1) {
      final String packageName = name.substring(0, packageIndex);
      final Package classPackage = getPackage(packageName);
      if (classPackage == null) {
        definePackage(packageName, null, null, null, null, null, null, null);
      }
    }
    byte[] classByte = new byte[(int)classFile.length()];
    FileInputStream fis = null;
    try {
      fis = new FileInputStream(classFile);
      fis.read(classByte);
      return defineClass(name, classByte, 0, classByte.length);
    } catch (IOException ex) {
      throw new ClassNotFoundException("Can't find class:" + name);
    } finally {
      IOUtils.close(fis);
    }
  }
}

class IOUtils {
  /**
   * find out all readable files under dir recursively
   */
  public static List<File> getAllFilesUnderDir(File dir, final FileFilter fileFilter) {
    FileFilter acceptDirFileFilterWrapper = new FileFilter() {
        public boolean accept(File f) {
          if (f.isDirectory()) { return true; }
          return fileFilter.accept(f);
        }
      };
    ArrayList<File> files = new ArrayList<File>();
    Stack<File> s = new Stack<File>();
    s.push(dir);
    File tmp = null;
    while (!s.isEmpty()) {
      tmp = s.pop();
      if (tmp.isDirectory() && tmp.canRead() && tmp.canExecute()) {
        File[] cs = tmp.listFiles(acceptDirFileFilterWrapper);
        for (File c : cs) {
          s.push(c);
        }
      } else if (tmp.isFile() && tmp.canRead()) {
        files.add(tmp);
      }
    }
    return files;
  }
  /**
   * delete file or directory recursively.
   */
  public static void del(File f) {
    if (f.exists() && f.isDirectory()) {
      if (f.listFiles().length == 0) {
        f.delete();
      } else {
        File delFile[] = f.listFiles();
        int i = f.listFiles().length;
        for (int j = 0; j < i; j++) {
          if (delFile[j].isDirectory()) {
            del(delFile[j]);
          }
          delFile[j].delete();
        }
      }
    }
  }

  public static void copy(File src, File dist) {
    try {
      if (!dist.getParentFile().exists()) { dist.getParentFile().mkdirs(); }
      FileInputStream fis = new FileInputStream(src);
      FileOutputStream fos = new FileOutputStream(dist);
      copy(fis, fos);
      IOUtils.close(fis);
      IOUtils.close(fos);
    } catch (IOException ex) {
      ex.printStackTrace();
    } finally {
    }
  }

  public static void copy(InputStream in, OutputStream out) {
    byte[] buf = new byte[1024];
    int count = -1;
    try {
      while ((count = in.read(buf)) != -1) {
        out.write(buf, 0, count);
      }
      out.flush();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public static void close(InputStream in) {
    if (in != null) {
      try {
        in.close();
        in = null;
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public static void close(OutputStream out) {
    try {
      out.flush();
      out.close();
      out = null;
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
