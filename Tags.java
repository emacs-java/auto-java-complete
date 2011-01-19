// Tags - make a tags table by reflection

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// So here’s how to generate the home directory’s tags file:
//     javac -d ~/ Tags.java
//     java -cp ~/:$JAVA_HOME/jre/lib/rt.jar Tags "java.*"
//  or java Tags 
// then it will generate a file ~/.java_base.tag in your home

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to the
// Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.


import java.io.*;
import java.lang.reflect.*;
import java.util.*;
import java.util.regex.*;
import java.util.jar.*;
import java.util.zip.*;


/** Make a tags table from the class files in a classpath.
 * The classpath is obtained from the property <code>java.class.path</code>
 *
 * The class names that get output must match the <code>packageFilter</code>
 * if it is specified.
 *
 * @author joseph <jixiuf@gmail.com>
 */
public class Tags {
    BufferedWriter tagFile=null;
    List<Class> clss= new LinkedList<Class>( );
    List<PackageItem> pkgs=new LinkedList<PackageItem>( );
    List<ClassItem> classes=new LinkedList<ClassItem>( );
    List<MemberItem> members=new LinkedList<MemberItem>( );
    String fileSeparator=System.getProperty("file.separator");
    int shift=6;
     public Tags(){
         try {
           tagFile = new BufferedWriter( new FileWriter ( new File( System.getProperty("user.home"),".java_base.tag"))) ;
         }catch ( Exception e ){
             System.err.print( e.getMessage( ));
         }
     }
  private void processJarFile (File f) throws PatternSyntaxException {
    try {
        JarFile jar = new JarFile(f);
        Enumeration en = jar.entries();
        int i = 0;
        while (en.hasMoreElements())
          {
            ZipEntry z = (ZipEntry) en.nextElement();
            String name = z.getName();
            if (name.indexOf(".class") < 0) continue;
            name= name.substring(0, name.lastIndexOf(".class"));
           String className= name.replace("/", ".");
           processClass(className);
          }
      } catch (IOException e) {
        System.err.println("Bad jar?");
      }
  }

    /**
       @param className  full className
       
     */
    private void processClass(String className){
            if (packageFilter != null && !(className.matches(packageFilter))) return;
            if ( className.startsWith( "sun")){
                return;
            }
            if ( className.startsWith( "com.sun")){
                return;
            }
           if ( className.startsWith( "com.thaiopensource")){
                return;
            }
            if ( className .contains( "org.iso_relax.ant")){
                return;
            }
            if ( className .contains( "$")){
                return;
            }
            try {
                Class c = Class.forName(className,true,ClassLoader.getSystemClassLoader()) ;
                clss.add( c);
            } catch (ClassNotFoundException e) {
                System.err.println("Class not found:" + className);
            } catch (NoClassDefFoundError e) { 
                System.err.println("Class not found:" + className);
            } catch (UnsatisfiedLinkError e) {
                System.err.println("Class's linkage failed:" + className);
            }
        
    }
    /**
     * find out all  readable files under dir recursively 
     */
	private List<File> getAllFilesUnderDir(File dir, final FileFilter fileFilter) {
        FileFilter acceptDirFileFilterWrapper  = new FileFilter(){
                public boolean accept(File f)   {
                    if (f.isDirectory()) return true;
                    return   fileFilter.accept(f);
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

  /** Find class file and jar  in the directory.
      
   */
  private void processDirectory (File dir) {
      if (dir!=null &&dir.isDirectory()){
          String dirFullPath=dir.getAbsolutePath();
          List<File> clazzFiles =getAllFilesUnderDir
              (dir, new FileFilter(){ public boolean accept(File f){
                                  if (f.getName().endsWith(".class")) return true;
                                  return false;
                              }});
               for(File clazz:clazzFiles){
                       String classAbsolutePath=clazz.getAbsolutePath();
                       String classFullName = classAbsolutePath
                                    .substring(dirFullPath.length()+1 , classAbsolutePath.indexOf(".class")  )
                           .replaceAll(fileSeparator,".");
                       processClass(classFullName);
               }
               File [] jarz=dir.listFiles(
                                          new FileFilter(){
                                              public boolean accept(File f ){
                                                  if (f.getName().endsWith(".jar")) return true;
                                                  return false;
                                              }
                                          }
              );
               if(jarz!=null){
                   for(File jarFile:jarz){
                       processJarFile(jarFile);
                   }
               }
      }
  }


  /** If this is not null it's used as a filter for acceptable classes.
   * Only packages that <code>matches(packageFilter)</code> will be tagged.
   */
  protected String packageFilter;
  

  private void processClasspath (String classpath) throws PatternSyntaxException {
      String [] cls=classpath.split(  System.getProperty( "path.separator"));
          for ( int i=0;i<cls.length;i++) {
              String element = cls[ i];
              if(element.equals(".") ||element.equals("./" )||element.equals(".\\")) continue;
              File f = new File(element);
              if (f.exists()){
                  if (f.isDirectory()) processDirectory(f); else  processJarFile(f);
              }
          }
          if(!classpath.contains( fileSeparator+ "rt.jar")){
              processJarFile(new File(System.getenv("JAVA_HOME")+fileSeparator+"jre"+fileSeparator+"lib"+
                                      fileSeparator+"rt.jar"));
              
          }
          tagAll( );
          write( );
          
  }
    private void  tagAll( ){
        for ( Class c :clss){ 
               tagClass( c);
        }
        Collections.sort( pkgs);
        Collections.sort( classes);
        int pkg_size=pkgs.size( );
        for( int i=0;i<pkg_size;i++){// now pkgs are sorted ,so the line num of the pkg in tag file  is the index+1 in pkgs  list
            pkgs.get( i).lineNum=shift+i+1;
        }
        int classes_size=classes.size( );
        PackageItem pkgItem=null;
        ClassItem cItem=null;
        for( int i=0;i<classes_size;i++){
  //the line number of class  in tag file is the count of packages plus the index of the class in classes list 
  // in this loop ,we will populte the lineNum of each ClassItem and populate the classStartLineNum and classEndLineNum of each package 
            cItem=classes.get( i);
            cItem.lineNum=shift+pkg_size+i+1;
            if ( i==0){
                pkgItem=cItem.pkgItem;
                pkgItem.classStartLineNum=cItem.lineNum;
            }else if( pkgItem!=cItem.pkgItem){
                pkgItem.classEndLineNum=cItem.lineNum;
                pkgItem=cItem.pkgItem;
                pkgItem.classStartLineNum=cItem.lineNum;
            }
        }
        pkgItem.classEndLineNum=cItem.lineNum+1; //populate the last pkgLast 
        pkgItem=null ; cItem=null;

        for( int i =0;i<classes_size;i++){ // in this loop ,we will populate basic info about each member of class into  (exclude )
               cItem =classes.get( i);
              cItem.memStartLineNum=shift+pkg_size+classes_size+members.size( )+1;
              tagConstructors( cItem);
              tagFields(cItem);
              tagMethods( cItem);
              cItem.memEndLineNum=shift+pkg_size+classes_size+members.size( );

        }
        int members_size=members.size( );
        MemberItem memItem=null;
        cItem=null;
        int memberLineNum_start=shift+pkg_size+classes_size+1;
        for( int i=0;i<members_size;i++){
            memItem=members.get( i);
            memItem.lineNum=memberLineNum_start+i;
            if ( i==0){
                cItem=memItem.cItem;
                cItem.memStartLineNum=memItem.lineNum;
                //            }else if( pkgItem!=cItem.pkgItem){
          }else if( cItem!=memItem.cItem){
                cItem.memEndLineNum=memItem.lineNum;
                cItem=memItem.cItem;
                cItem.memEndLineNum=memItem.lineNum;
            }
        }
        cItem.memEndLineNum=memItem.lineNum+1;
        memItem=null; cItem=null;
    }

    // analyze  Class c ,and populate PackageItem with  its  package info ,and populate ClassItem with its class info 
    //then add them to classes and pkgs list 
    private void tagClass(Class c){
        if( c.isAnnotation( )) return ;
        if( c.isAnonymousClass() ) return ;
        if( c.isArray( )) return;
        if( c.isPrimitive() ) return;
        if( c.getSimpleName( ).equals( "")) return;
        if( !Modifier.isPublic( c.getModifiers( ))) return;

        String pkgName=c.getPackage( ).getName( );
             PackageItem pkgItem =null;
        for( int i=0;i<pkgs.size( );i++){
            if ( pkgs.get( i).name.equals( pkgName)  ) {
                pkgItem= pkgs.get( i);
                break;
            }
        }
        if ( pkgItem==null){
            pkgItem= new PackageItem( );
            pkgItem.name=pkgName;
            pkgs.add( pkgItem);
        }
        ClassItem cItem= new ClassItem( );
        cItem.cls=c;
        cItem.name=c.getSimpleName( );
        cItem.pkgItem=pkgItem;
       for( ClassItem ci:classes){
            if( ci.equals(cItem )) return ;
        }
        classes.add( cItem);
  }
    //maybe there are bugs here ,i think i should write it depend on different type ,like annotation enum and so on
    private PkgClsKV getPkgClsKV( Class type){
        PkgClsKV returnType = new PkgClsKV( );
        if ( type.isPrimitive( )){
            returnType.alternativeString=type.getName( );
        } else if( type.isArray( )){
             returnType.alternativeString=type.getName( );
             String typeName=type.getName( );
            if(typeName.contains( "[I")){
                String tmp="int";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
            }else if ( typeName.contains( "[F")){
                String tmp="float";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
            }else if  ( typeName.contains( "[Z")){
                String tmp="boolean";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
            }else  if (  typeName.contains( "[J")){
                String tmp="long";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
            }else if ( typeName.contains ( "[B")){
                String tmp="byte";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
            }else if  ( typeName.contains("[C")){
                String tmp="char";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
            }else if  (typeName.contains("[S")){
                String tmp="char";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
             }else if  ( typeName.contains("[D")){
                 String tmp="";
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) tmp+="[]";
                }
                returnType.alternativeString=tmp;
             }else if (typeName.contains( "[L") ){
                int index =(  typeName.indexOf( "[L"));
                String className=typeName.substring( index+2,typeName.length( )-1);
                for( int i=0;i<typeName.length( );i++){
                    if( typeName.charAt( i) =='[' ) className+="[]";
                }
                returnType.alternativeString=className;
            }
        }  else if ( type.isAnnotation( )){
              returnType.alternativeString=type.getName( );
            //  do nothing
        } else if ( type.isEnum( )){
              returnType.alternativeString=type.getName( );
        }else {
            for ( ClassItem ci:classes){
                if( type.getName( )!=null &&type.getName( ).equals(ci.cls.getName( ) )){
                    returnType.cItem=ci;
                    break;
                }
            }
            if( returnType.cItem==null){
                returnType.alternativeString=type.getName( );
            }
        }
        return returnType;
    }
      //tag Field 
    private void tagFields(ClassItem cItem) {
    try {
          Field[ ] fields =cItem.cls.getDeclaredFields();
          for (int i = 0; i < fields.length; i++){
              if ( !Modifier.isPublic(fields[ i].getModifiers( ))){
                  continue;
              }
              Class fieldType=( Class)   fields[ i].getType( );
              PkgClsKV returnType=getPkgClsKV(fieldType);
              MemberItem memItem= new MemberItem( );
              memItem.cItem=cItem;
              memItem.name=fields[ i].getName();
              memItem.returnType =returnType;
              memItem.field=fields[ i];
              members.add( memItem);
          }
    } catch (NoClassDefFoundError e) {
        System.err.println(cItem.cls.getName() + " is not found.");
    }catch (Exception e) {
        System.err.println(e.getMessage());
    }
  }
  private void tagConstructors(ClassItem cItem) {
    try {
        Constructor[] methods = cItem.cls.getDeclaredConstructors();
        for (int i = 0; i < methods.length; i++)
          try {
                if (! Modifier.isPublic( methods[ i].getModifiers( ))) continue; 
                MemberItem memItem  = new MemberItem( );
                memItem.constructor=methods[ i];
                String name=methods[i ].getName( ) ;
                if( name.contains( ".")){
                    memItem.name=name.substring( name.lastIndexOf("." )+1);
                }else{
                    memItem.name=name;
                }
                memItem.cItem=cItem;
                Class[] params = methods[i].getParameterTypes();
                 List<PkgClsKV> paramsKV=new ArrayList<PkgClsKV>( );
                for( Class param:params) paramsKV.add( getPkgClsKV( param));
                 memItem.params=paramsKV;
                 
                 Class[ ] exceptions= methods[ i].getExceptionTypes( );
                 List<PkgClsKV> exceptionsKV=new ArrayList<PkgClsKV>( );
                for( Class e:exceptions) exceptionsKV.add( getPkgClsKV(e));
                 memItem.exceptions=exceptionsKV;
                 members.add( memItem);
            } catch (IllegalStateException e) {e.printStackTrace( ); }
    } catch (NoClassDefFoundError e) { e.printStackTrace( );
    } catch (Exception e) { e.printStackTrace( ); }
  }
  private void tagMethods(ClassItem cItem) {
   try {
       //        Method[] methods = cItem.cls.getDeclaredMethods();
       Method[] methods = cItem.cls.getMethods();
        for (int i = 0; i < methods.length; i++){
            try {
                if (! Modifier.isPublic( methods[ i].getModifiers( ))) continue; 
                MemberItem memItem  = new MemberItem( );
                memItem.method=methods[ i];
                memItem.name=methods[ i].getName();
                memItem.cItem=cItem;
                memItem.returnType=getPkgClsKV( methods[ i].getReturnType( ));
                
                Class[] params = methods[i].getParameterTypes();
                 List<PkgClsKV> paramsKV=new ArrayList<PkgClsKV>( );
                for( Class param:params) paramsKV.add( getPkgClsKV( param));
                 memItem.params=paramsKV;
                 
                 Class[ ] exceptions= methods[ i].getExceptionTypes( );
                 List<PkgClsKV> exceptionsKV=new ArrayList<PkgClsKV>( );
                for( Class e:exceptions) exceptionsKV.add( getPkgClsKV(e));
                 memItem.exceptions=exceptionsKV;
                 members.add( memItem);
              } catch (IllegalStateException e) {e.printStackTrace( ); }// Throw this away because it means the constructor was private  
        }
    } catch (NoClassDefFoundError e) {
        System.err.println(cItem.cls.getName() + " is not found.");
    } catch (Exception e) {
        System.err.println(e.getMessage());
    }
  }
 
    private void write( ){
        try{
            tagFile.append( "don't try to edit this file ,even this line!!!!") ;tagFile.newLine( );
            tagFile.append( "package count="+pkgs.size( ) +"  ,Class count="+classes.size( ) +" , member count( constructor, field, method )= "+members.size( )  );tagFile.newLine( );
            tagFile.append(""+ (shift+1));tagFile.newLine( );
            tagFile.append( ""+(shift+pkgs.size( )+1));tagFile.newLine( );
            tagFile.append(""+ (shift+pkgs.size( )+classes.size( )+1)); tagFile.newLine( );
            tagFile.append(""+ ( shift+pkgs.size( ) +classes.size( )+members.size()+1  ));tagFile.newLine( );
            int i=0; 
        for( PackageItem pkgItem:pkgs){
            tagFile.append( pkgItem.toString( ));
            tagFile.newLine( );
            if ( i%300==0  ) tagFile.flush( );
            i++;
        }
        tagFile.flush( );
        i=0;
        for( ClassItem cItem:classes){
            tagFile.append( cItem.toString( ));
            tagFile.newLine( );
            if ( i%300==0  ) tagFile.flush( );
            i++;
        }
        tagFile.flush( );
        i=0;
        for( MemberItem mi:members){
            tagFile.append( mi.toString( ));
            tagFile.newLine( );
            if ( i%300==0  ) tagFile.flush( );
            i++;
        }

        tagFile.flush( );
        }catch( Exception e ){
            System.err.println( e.getMessage( ));
        }finally{
            try{ tagFile.close( ); tagFile=null; }catch(Exception e){e.printStackTrace( );}
        }

    }

  
  public static void main (String[] argv) throws Exception {
System.out.println( 
"*****************************************************************\n"+
"**       this program will need about 3 to 5 min ,            **\n"+
"**       before it exit,you may see a few exceptions           **\n" +
"**       if it don't kill the program ,just ignore it .        **\n" +
"*****************************************************************\n"
);
System.out.println(
"**************************************************************************\n"+
"***     you can use this Class like this:                              ***\n"+                   
"***       java Tags com.company.*                                      ***\n"+
"***       java -cp yourclasspath Tags                                  ***\n"+
"***only those package  name  starts with com.company  will be tagged.  ***\n"+
"***or all jar file in $CLASSPATH will be tagged .                      ***\n"+
"***before that you'd better backup the  file ~/.java_base.tag,if exists***\n"+
"***after that ,you can add both of them to you emacs config file       ***\n"+
"***   actually now only support one tag file                           ***\n"+
"**************************************************************************\n"
);
    Tags tags = new Tags();
    if (argv.length > 0) tags.packageFilter = argv[0];
    tags.processClasspath(System.getProperty("java.class.path")) ;
 
    System.out.println( 
"***********************************************************************\n"+
"***                  exit successful!!!                             ***\n"+
"***you will see a file named '.java_base.tag' in your home directory***\n"+
"***********************************************************************\n"
);
    System.exit(0);
  }
}
class PackageItem implements Comparable<PackageItem>{
    String name;
    int lineNum;
    int classStartLineNum;
    int classEndLineNum;
    Package pkg;
   public int compareTo(PackageItem pkgItem){
       return this.name.compareTo( pkgItem.name);
    }
   public String toString( ){
       return this.name+"`"+this.classStartLineNum+"`"+this.classEndLineNum;
   }
}
class ClassItem implements Comparable <ClassItem> {
    String name ;
    int lineNum;
    PackageItem pkgItem;
    int memStartLineNum;
    int memEndLineNum;
    Class cls;
   public int compareTo(ClassItem cItem ){
       int pkgCmp=this.pkgItem.name.compareTo( cItem.pkgItem.name);
       if(  pkgCmp!=0) return pkgCmp;
       return ( this.name.compareTo( cItem.name));
    }
   public int hashCode( ){
       if (this.pkgItem==null||this.pkgItem.name==null) return this.name.hashCode( );
       return (this.name+"."+this.pkgItem.name).hashCode( );
   }
   public boolean equals( Object obj){
       if ( obj==null ) return false;
       if(!(obj instanceof ClassItem )) return false;
       ClassItem other=( ClassItem) obj;
       if( this.pkgItem==null &&other.pkgItem==null &&this.name!=null) return this.name.equals( other.name);
       if( this.pkgItem!=null&&other.pkgItem!=null&&this.name!=null&&this.name.equals( other.name) &&this.pkgItem.name!=null&&this.pkgItem.name.equals(other.pkgItem.name )){
           return true;
       }
       return false;
   }
   public String toString( ){
    return this.name+"`"+this.pkgItem.lineNum+"`"+this.memStartLineNum+"`"+this.memEndLineNum;
   }


}
class MemberItem implements Comparable<MemberItem>{
    String name;
    ClassItem cItem;
    int lineNum;
    List<PkgClsKV> params;
    List<PkgClsKV> exceptions ;
    PkgClsKV returnType;
    Field field;
    Method method;
    Constructor constructor;
    public String toString( ){
        StringBuffer returnStr=new StringBuffer( );
        if ( constructor!=null ){
            returnStr.append( "  "+name+"`");
            //append params
            if ( params!=null){
                for( PkgClsKV param:params){
                    if( param.alternativeString!=null) returnStr.append( "~"+param.alternativeString+",");
                    else  returnStr.append(param.cItem.lineNum+",");
                }
                if(params.size( )>0) returnStr.deleteCharAt(returnStr.length( )-1 );
            }
            returnStr.append( "`");
            //append exceptions
            if ( exceptions!=null){
                for( PkgClsKV exp:exceptions){
                    if( exp.alternativeString!=null) returnStr.append( "~"+exp.alternativeString+",");
                    else  returnStr.append(exp.cItem.lineNum+",");
                }
                if(  exceptions.size( )>0) returnStr.deleteCharAt(returnStr.length( )-1 );
            }
        }else if( field!=null){
            returnStr.append(" "+name+"`");
            //    returnStr.append( cItem.lineNum+"`");
            //apend the field type
            if( returnType.alternativeString!=null) returnStr.append("~"+returnType.alternativeString );
            else returnStr.append(returnType.cItem.lineNum );
        }else if ( method!=null){
            returnStr.append( name+"`");
            //append returnType
            if( returnType.alternativeString!=null) {
                     returnStr.append("~"+returnType.alternativeString );
            } else {
                if( returnType.cItem==null){
                  System.out.println( "mem.name="+name );
                  System.out.println( method.getDeclaringClass( ).getName( ));
                    
                }
                     returnStr.append(returnType.cItem.lineNum );
            }
            returnStr.append( "`");
            //append params
            if ( params!=null){
                for( PkgClsKV param:params){
                    if( param.alternativeString!=null) returnStr.append( "~"+param.alternativeString+",");
                    else  returnStr.append(param.cItem.lineNum+",");
                }
                if(params.size( )>0) returnStr.deleteCharAt(returnStr.length( )-1 );
            }
            returnStr.append( "`");
            //append exceptions
            if ( exceptions!=null){
                for( PkgClsKV exp:exceptions){
                    if( exp.alternativeString!=null) returnStr.append( "~"+exp.alternativeString+",");
                    else  returnStr.append(exp.cItem.lineNum+",");
                }
                if(  exceptions.size( )>0) returnStr.deleteCharAt(returnStr.length( )-1 );
            }
            
        }
     return returnStr.toString( );
    }
    public int compareTo( MemberItem memItem){
       int classCompareResult=cItem.compareTo( memItem.cItem);
       if ( classCompareResult!=0){
           return classCompareResult;
       }
       if ( this.field!=null){
            if ( memItem.field==null) return -1;
            return this.name.compareTo( memItem.name);
       }else {
           if(memItem.method==null)  return 1;
            return this.name.compareTo( memItem.name);
       }
        
    }
}
class  PkgClsKV{ 
     ClassItem cItem;
     String alternativeString;
}
