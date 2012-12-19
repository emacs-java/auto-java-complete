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


/** Make a tags table from the class files in a classpath.
 * The classpath is obtained from the property <code>java.class.path</code>
 *
 * The class names that get output must match the <code>classExcludeRegexPatternArray</code>
 * if it is specified.
 *
 * @author joseph <jixiuf@gmail.com>
 */
public class Tags {
  private BufferedWriter _tagFile = null;
  private PrintWriter _logError = null;
  private PrintWriter _logInfo = null;
  private List<Class> _clss = new LinkedList<Class>();
  private List<PackageItem> _pkgs = new LinkedList<PackageItem>();
  private List<ClassItem> _classes = new LinkedList<ClassItem>();
  private List<MemberItem> _members = new LinkedList<MemberItem>();
  private String _fileSeparator = System.getProperty("file.separator");
  private int _shift = 6;
  protected Pattern[] _classExcludeRegexPatternArray = null;
  private File _randomTmpPath = new File(System.getProperty("java.io.tmpdir") +
                                         File.separatorChar +
                                         UUID.randomUUID().toString() +
                                         File.separatorChar);
  private ClassLoader _cl = new CL(_randomTmpPath);

  public Tags() {
    try {
      _tagFile = new BufferedWriter(new FileWriter(new File(getHomePath(), ".java_base.tag"))) ;
      _logError = new PrintWriter(new File(System.getProperty("java.io.tmpdir"), "ajc_error.log")) ;
      _logInfo = new PrintWriter(new File(System.getProperty("java.io.tmpdir"), "ajc_info.log")) ;
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
  private void clear() {
    if (_randomTmpPath != null && _randomTmpPath.isDirectory()) {
      IOUtils.del(_randomTmpPath);
      _randomTmpPath.delete();
    }
  }

  // Entry point and called from TagsMain
  public void process() {
    prepare();//copy or unzip *.class *.jar to _randomTmpPath
    if (_randomTmpPath != null && _randomTmpPath.isDirectory()) {
      System.out.println("tmp classpath :" + _randomTmpPath.getAbsolutePath());
      String dirFullPath = _randomTmpPath.getAbsolutePath();
      List<File> clazzFiles = IOUtils.getAllFilesUnderDir
        (_randomTmpPath, new FileFilter() {
            public boolean accept(File f) {
              if (f.getName().endsWith(".class")) { return true; }
              return false;
            }
          });
      for (File clazz : clazzFiles) {
        String classAbsolutePath = clazz.getAbsolutePath();
        String classFullName = classAbsolutePath
          .substring(dirFullPath.length() + 1 , classAbsolutePath.indexOf(".class"))
          .replace(_fileSeparator, ".");
        processClass(classFullName);
      }
      tagAll();
      write();
    }
    clear();
  }

  private void processJarFile(File f) {
    Unzip.unzip(f, _randomTmpPath);
    System.out.println("adding " + f.getAbsolutePath() + "  to classpath...");
    // JarFile jar=null;
    // try { jar = new JarFile(f); } catch (IOException e) {log(e);}
    // Enumeration en = jar.entries();
    // int i = 0;
    // while (en.hasMoreElements()){
    //     ZipEntry z = (ZipEntry) en.nextElement();
    //     String name = z.getName();
    //     if (name.indexOf(".class") < 0) continue;
    //     name= name.substring(0, name.lastIndexOf(".class"));
    //     String className= name.replace("/", ".");
    //     processClass(className);
    // }
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
        // String classFullName = classAbsolutePath
        //     .substring(dirFullPath.length()+1 , classAbsolutePath.indexOf(".class"))
        //     .replace(fileSeparator,".");
        // processClass(classFullName);
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
      // File [] jarz=dir.listFiles(
      //                            new FileFilter(){
      //                                public boolean accept(File f){
      //                                    if (f.getName().endsWith(".jar")) return true;
      //                                    return false;
      //                                }
      //                            }
      //                            );
      if (jarz != null) {
        for (File jarFile: jarz) {
          processJarFile(jarFile);
        }
      }
    }
  }

  // Process each class in _clss
  private void tagAll() {
    System.out.println("found " + _clss.size() + "  classes.");
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

      Collections.sort(_pkgs);
      Collections.sort(_classes);
      System.out.println("tagged " + _classes.size() + "  classes.");

      for (ClassItem cItem : _classes) {
        try {
          List<MemberItem> localMems = tagConstructors(cItem);
          localMems.addAll(tagMethods(cItem));
          localMems.addAll(tagFields(cItem));
          cItem._members = localMems;
        } catch (ApplicationException e) {
          logInfo(e.getMessage());
        } catch (Throwable ex) {
          log(ex);
        }
      }
      int pkg_size = _pkgs.size();
      int classes_size = _classes.size();
      PackageItem pkgItem = null;
      ClassItem cItem = null;

      for (int i = 0; i < pkg_size; i++) {
        // now pkgs are sorted ,so the line num of the pkg in tag file is the index+1 in pkgs list
        _pkgs.get(i)._lineNum = _shift + i + 1;
      }
      for (int i = 0; i < classes_size; i++) {
        //the line number of class in tag file is the count of packages
        // plus the index of the class in classes list
        // in this loop ,we will populte the lineNum of each ClassItem and
        // populate the classStartLineNum and classEndLineNum of each package
        cItem = _classes.get(i);
        cItem._lineNum = _shift + pkg_size + i + 1;
        if (i == 0) {
          pkgItem = cItem._pkgItem;
          pkgItem._classStartLineNum = cItem._lineNum;
        } else if (pkgItem != cItem._pkgItem) {
          pkgItem._classEndLineNum = cItem._lineNum;
          pkgItem = cItem._pkgItem;
          pkgItem._classStartLineNum = cItem._lineNum;
        }
      }
      if (cItem != null && pkgItem != null) {
        //TODO: cItem maybe null here ,bugfix
        pkgItem._classEndLineNum = cItem._lineNum + 1; //populate the last pkgLast
      }
      pkgItem = null ;
      cItem = null;

      for (int i = 0; i < classes_size; i++) {
        // in this loop ,we will populate basic info about each member of class into  (exclude)
        cItem = _classes.get(i);
        cItem._memStartLineNum = _shift + pkg_size + classes_size + _members.size() + 1;
        if (cItem._members != null) {
          _members.addAll(cItem._members);
          cItem._memEndLineNum = cItem._memStartLineNum + cItem._members.size() - 1;
          cItem._members = null;
        }
      }

      cItem = null;

      MemberItem memItem = null;
      int members_size = _members.size();
      int memberLineNum_start = _shift + pkg_size + classes_size + 1;
      for (int i = 0; i < members_size; i++) {
        memItem = _members.get(i);
        memItem._lineNum = memberLineNum_start + i;
        if (i == 0) {
          cItem = memItem._cItem;
          cItem._memStartLineNum = memItem._lineNum;
        } else if (cItem != memItem._cItem) {
          cItem._memEndLineNum = memItem._lineNum;
          cItem = memItem._cItem;
          cItem._memEndLineNum = memItem._lineNum;
        }
      }
      if (cItem != null && memItem != null) {
        cItem._memEndLineNum = memItem._lineNum + 1;
      }
      memItem = null;
      cItem = null;
    } catch (Throwable t) {
      log(t);
    }
  }

  private void checkClassToExclude(Class c) throws ApplicationException {
    if (c.isAnonymousClass()) {
      throw new ApplicationException("sorry, you are an AnnonymousClass:" + c.getName());
    }
    if (c.isArray()) {
      throw new ApplicationException("sorry, you are Array:" + c.getName());
    }
    if (c.isPrimitive()) {
      throw new ApplicationException("sorry you are a Primitive type:" + c.getName());
    }
    if (c.getSimpleName().equals("")) {
      throw new ApplicationException("why don't you have a name ,I don't know how to handle you:" +
                                     c.getName());
    }
    if ((!Modifier.isPublic(c.getModifiers())) &&
        (!c.isInterface()) &&
        (!Modifier.isAbstract(c.getModifiers()))) {
      throw new ApplicationException("sorry ,you are not a public class:" + c.getName());
    }
    if (c.getPackage() == null) {
      throw new ApplicationException("why don't you hava a package name?:" + c.getName());
    }
  }

  // analyze Class c, and populate PackageItem with its package info,
  // and populate ClassItem with its class info,
  // then add them to _classes and _pkgs list
  private ClassItem tagClass(Class c) throws ApplicationException {
    checkClassToExclude(c);
    String pkgName = c.getPackage().getName();
    if (c.isAnnotation() && c.getName().contains("$")) {
      pkgName = c.getName().substring(0 , c.getName().lastIndexOf('$'));
    }
    PackageItem pkgItem = null;
    // check if pkgName is in list.
    for (int i = 0; i < _pkgs.size(); i++) {
      if (_pkgs.get(i)._name.equals(pkgName)) {
        pkgItem = _pkgs.get(i);
        break;
      }
    }
    // if there's no such pkgItem, we create a new one and insert it
    // into list.
    if (pkgItem == null) {
      pkgItem = new PackageItem();
      pkgItem._name = pkgName;
      _pkgs.add(pkgItem);
    }
    // then we create a new ClassItem and return it.
    ClassItem cItem = new ClassItem();
    cItem._cls = c;
    cItem._name = c.getSimpleName();
    cItem._pkgItem = pkgItem;
    for (ClassItem ci : _classes) {
      if (ci.equals(cItem)) {
        throw new ApplicationException("you have already in, why come here again! :" + c.getName());
      }
    }
    return cItem;
  }

  // maybe there are bugs here, i think i should write it depend on
  // different type, like annotation enum and so on
  private ClassItemWrapper getClassItemWrapper(Class type) {
    ClassItemWrapper returnType = new ClassItemWrapper();
    if (type.isPrimitive()) {
      returnType._alternativeString = type.getName();
    } else if (type.isArray()) {
      returnType._alternativeString = type.getName();
      String typeName = type.getName();
      if (typeName.contains("[I")) {
        String tmp = "int";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[F")) {
        String tmp = "float";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[Z")) {
        String tmp = "boolean";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[J")) {
        String tmp = "long";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[B")) {
        String tmp = "byte";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[C")) {
        String tmp = "char";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[S")) {
        String tmp = "char";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[D")) {
        String tmp = "";
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { tmp += "[]"; }
        }
        returnType._alternativeString = tmp;
      } else if (typeName.contains("[L")) {
        int index = (typeName.indexOf("[L"));
        String className = typeName.substring(index + 2, typeName.length() - 1);
        for (int i = 0; i < typeName.length(); i++) {
          if (typeName.charAt(i) == '[') { className += "[]"; }
        }
        returnType._alternativeString = className;
      }
    } else if (type.isAnnotation()) {
      returnType._alternativeString = type.getName();
      // do nothing
    } else if (type.isEnum()) {
      returnType._alternativeString = type.getName();
    } else {
      for (ClassItem ci : _classes) {
        if (type.getName() != null && type.getName().equals(ci._cls.getName())) {
          returnType._cItem = ci;
          break;
        }
      }
      if (returnType._cItem == null) {
        returnType._alternativeString = type.getName();
      }
    }
    return returnType;
  }

  //tag Field
  private List<MemberItem> tagFields(ClassItem cItem) throws Throwable {
    Field[] fields = cItem._cls.getDeclaredFields();
    List<MemberItem> localMems = new ArrayList<MemberItem>();
    for (int i = 0; i < fields.length; i++) {
      if (!Modifier.isPublic(fields[i].getModifiers())) {
        continue;
      }
      Class fieldType = (Class)fields[i].getType();
      ClassItemWrapper returnType = getClassItemWrapper(fieldType);
      MemberItem memItem = new MemberItem();
      memItem._cItem = cItem;
      memItem._name = fields[i].getName();
      memItem._returnType = returnType;
      memItem._field = fields[i];
      localMems.add(memItem);
    }
    Collections.sort(localMems);
    return localMems;
  }

  private List<MemberItem> tagConstructors(ClassItem cItem) throws Throwable {
    Constructor[] methods = cItem._cls.getDeclaredConstructors();
    List<MemberItem> localMems = new ArrayList<MemberItem>();
    for (int i = 0; i < methods.length; i++) {
      if (!Modifier.isPublic(methods[i].getModifiers())) { continue; }
      MemberItem memItem = new MemberItem();
      memItem._constructor = methods[i];
      String name = methods[i].getName() ;
      if (name.contains(".")) {
        memItem._name = name.substring(name.lastIndexOf(".") + 1);
      } else {
        memItem._name = name;
      }
      memItem._cItem = cItem;
      Class[] params = methods[i].getParameterTypes();
      List<ClassItemWrapper> paramsKV = new ArrayList<ClassItemWrapper>();
      for (Class param : params) {
        paramsKV.add(getClassItemWrapper(param));
      }
      memItem._params = paramsKV;

      Class[] exceptions = methods[i].getExceptionTypes();
      List<ClassItemWrapper> exceptionsKV = new ArrayList<ClassItemWrapper>();
      for (Class e : exceptions) {
        exceptionsKV.add(getClassItemWrapper(e));
      }
      memItem._exceptions = exceptionsKV;
      localMems.add(memItem);
    }
    Collections.sort(localMems);
    return localMems;
  }

  private List<MemberItem> tagMethods(ClassItem cItem) throws Throwable {
    // Method[] methods = cItem.cls.getDeclaredMethods();
    Method[] methods = cItem._cls.getMethods();
    List<MemberItem> localMems = new ArrayList<MemberItem>();
    for (int i = 0; i < methods.length; i++) {
      if (!Modifier.isPublic(methods[i].getModifiers())) { continue; }
      MemberItem memItem = new MemberItem();
      memItem._method = methods[i];
      memItem._name = methods[i].getName();
      memItem._cItem = cItem;
      memItem._returnType = getClassItemWrapper(methods[i].getReturnType());

      Class[] params = methods[i].getParameterTypes();
      List<ClassItemWrapper> paramsKV = new ArrayList<ClassItemWrapper>();
      for (Class param: params) { paramsKV.add(getClassItemWrapper(param)); }
      memItem._params = paramsKV;

      Class[] exceptions = methods[i].getExceptionTypes();
      List<ClassItemWrapper> exceptionsKV = new ArrayList<ClassItemWrapper>();
      for (Class e: exceptions) { exceptionsKV.add(getClassItemWrapper(e)); }
      memItem._exceptions = exceptionsKV;
      localMems.add(memItem);
    }
    Collections.sort(localMems);
    return localMems;
  }

  private void write() {
    try {
      _tagFile.append("don't try to edit this file ,even this line!!!!") ;
      _tagFile.newLine();
      _tagFile.append("package count=" + _pkgs.size() + "  ,Class count=" + _classes.size() + " , member count(constructor, field, method)= " + _members.size());
      _tagFile.newLine();
      _tagFile.append("" + (_shift + 1));
      _tagFile.newLine();
      _tagFile.append("" + (_shift + _pkgs.size() + 1));
      _tagFile.newLine();
      _tagFile.append("" + (_shift + _pkgs.size() + _classes.size() + 1));
      _tagFile.newLine();
      _tagFile.append("" + (_shift + _pkgs.size() + _classes.size() + _members.size() + 1));
      _tagFile.newLine();
      int i = 0;
      for (PackageItem pkgItem : _pkgs) {
        _tagFile.append(pkgItem.toString());
        _tagFile.newLine();
        if (i % 300 == 0) { _tagFile.flush(); }
        i++;
      }
      _tagFile.flush();
      i = 0;
      for (ClassItem cItem : _classes) {
        _tagFile.append(cItem.toString());
        _tagFile.newLine();
        if (i % 300 == 0) { _tagFile.flush(); }
        i++;
      }
      _tagFile.flush();
      i = 0;
      for (MemberItem mi : _members) {
        _tagFile.append(mi.toString());
        _tagFile.newLine();
        if (i % 300 == 0) { _tagFile.flush(); }
        i++;
      }
      _tagFile.flush();
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
}

class PackageItem implements Comparable<PackageItem> {
  String _name;
  int _lineNum;
  int _classStartLineNum;
  int _classEndLineNum;
  Package _pkg;
  public int compareTo(PackageItem pkgItem) {
    return _name.compareTo(pkgItem._name);
  }
  public String toString() {
    return _name + "`" + _classStartLineNum + "`" + _classEndLineNum;
  }
}

class ClassItem implements Comparable<ClassItem> {
  String _name;
  int _lineNum;
  int _memStartLineNum;
  int _memEndLineNum;
  Class _cls;
  PackageItem _pkgItem;
  List<MemberItem> _members;

  public int compareTo(ClassItem cItem) {
    int pkgCmp = _pkgItem._name.compareTo(cItem._pkgItem._name);
    if (pkgCmp != 0) { return pkgCmp; }
    return (_name.compareTo(cItem._name));
  }

  public int hashCode() {
    if (_pkgItem == null || _pkgItem._name == null) { return _name.hashCode(); }
    return (_name + "." + _pkgItem._name).hashCode();
  }

  public boolean equals(Object obj) {
    if (obj == null) { return false; }
    if (!(obj instanceof ClassItem)) { return false; }
    ClassItem other = (ClassItem)obj;
    if (_pkgItem == null && other._pkgItem == null && _name != null) {
      return _name.equals(other._name);
    }
    if (_pkgItem != null && other._pkgItem != null && _name != null &&
        _name.equals(other._name) && _pkgItem._name != null &&
        _pkgItem._name.equals(other._pkgItem._name)) {
      return true;
    }
    return false;
  }
  public String toString() {
    return _name + "`" + _pkgItem._lineNum + "`" + _memStartLineNum + "`" + _memEndLineNum;
  }
}

class MemberItem implements Comparable<MemberItem> {
  String _name;
  ClassItem _cItem;
  int _lineNum;
  List<ClassItemWrapper> _params;
  List<ClassItemWrapper> _exceptions ;
  ClassItemWrapper _returnType;
  Field _field;
  Method _method;
  Constructor _constructor;

  public String toString() {
    StringBuffer returnStr = new StringBuffer();
    if (_constructor != null) {
      returnStr.append("  " + _name + "`");
      //append params
      if (_params != null) {
        for (ClassItemWrapper param: _params) {
          if (param._alternativeString != null) {
            returnStr.append("~" + param._alternativeString + ",");
          } else {
            returnStr.append(param._cItem._lineNum + ",");
          }
        }
        if (_params.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
      returnStr.append("`");
      //append exceptions
      if (_exceptions != null) {
        for (ClassItemWrapper exp : _exceptions) {
          if (exp._alternativeString != null) {
            returnStr.append("~" + exp._alternativeString + ",");
          } else {
            returnStr.append(exp._cItem._lineNum + ",");
          }
        }
        if (_exceptions.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
    } else if (_field != null) {
      returnStr.append(" " + _name + "`");
      //    returnStr.append(cItem.lineNum+"`");
      //apend the field type
      if (_returnType._alternativeString != null) {
        returnStr.append("~" + _returnType._alternativeString);
      } else {
        returnStr.append(_returnType._cItem._lineNum);
      }
    } else if (_method != null) {
      returnStr.append(_name + "`");
      //append returnType
      if (_returnType._alternativeString != null) {
        returnStr.append("~" + _returnType._alternativeString);
      } else {
        if (_returnType._cItem == null) {
          System.out.println("mem.name=" + _name);
          System.out.println(_method.getDeclaringClass().getName());
        }
        returnStr.append(_returnType._cItem._lineNum);
      }
      returnStr.append("`");
      //append params
      if (_params != null) {
        for (ClassItemWrapper param : _params) {
          if (param._alternativeString != null) {
            returnStr.append("~" + param._alternativeString + ",");
          } else {
            returnStr.append(param._cItem._lineNum + ",");
          }
        }
        if (_params.size() > 0) { returnStr.deleteCharAt(returnStr.length() - 1); }
      }
      returnStr.append("`");
      //append exceptions
      if (_exceptions != null) {
        for (ClassItemWrapper exp : _exceptions) {
          if (exp._alternativeString != null) {
            returnStr.append("~" + exp._alternativeString + ",");
          } else {
            returnStr.append(exp._cItem._lineNum + ",");
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
  protected ClassItem _cItem;
  String _alternativeString;
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
  // public static void main(String[] args) {
  //     // TODO Auto-generated method stub
  //     Unzip z = new Unzip();
  //     z.unzip("/tmp/a.zip", "/tmp/d/");
  // }
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
    final int packageIndex = name.lastIndexOf('.') ;
    if (packageIndex != -1) {
      final String packageName = name.substring(0, packageIndex) ;
      final Package classPackage = getPackage(packageName) ;
      if (classPackage == null) {
        definePackage(packageName, null, null, null, null, null, null, null) ;
      }
    }
    byte[] classByte = new byte[(int) classFile.length()];
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
