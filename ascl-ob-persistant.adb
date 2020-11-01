--|
--| Filename        : $Source: /home/erdmann/ASCL/Library/RCS/ascl-ob-persistant.adb,v $
--| Description     : Component Template
--| Author          : Michael Erdmann
--| Created On      : 25.3.1999
--| Last Modified By: $Author: erdmann $
--| Last Modified On: $Date: 1999/08/21 14:40:31 $
--| Status          : $State: Exp $
--|
--| Functional Description
--| ======================
--| The component is based upon the following data item
--|
--|    Objects
--|    Pools
--|    Classes
--|
--| An object is an instance of a type which is derived from
--| this package. The Object it self is identified by a
--| so called object identikfier which is supplied by the
--| application.
--|
--| This package maintans a list of all classes which are derived
--| from this class. This table maintains a pointer to a so called
--| pool.
--| The object pool represents the file, where the objects are
--| stored. The pool maintains two files. One file contains the
--| object information like object id and location in the data
--| file and the data file where the objects it self is located.
--|
--| During installation of a new class of persitant objects,
--| the new data type is registerd with the pool identifier in
--| the class table.
--| Upon instanciation of a object the initialize procedure
--| is called and object is retrieved from the data file
--| or a new entry is created.
--|
--| Restrictions
--| ============
--| There is no automatic migration, if the the structure of
--| a persitant object is changed.
--|
--| References
--| ==========
--|
--|
--| History
--| =======
--| $Log: ascl-ob-persistant.adb,v $
--| Revision 1.7  1999/08/21 14:40:31  erdmann
--| First tested Version
--|
--| Revision 1.6  1999/08/15 19:59:04  erdmann
--| test version based upon controlled types
--|
--|
--|

--* Ada
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Tags;                     use Ada.Tags;
with Ada.Streams.Stream_IO;        use Ada.Streams.Stream_IO;
with Unchecked_Deallocation;
with Ada.Text_IO;                  use Ada;

with ASCL.Debugging_Support;       use ASCL.Debugging_Support;
use  ASCL;

package body ASCL.OB.Persistant is

   Version : constant String := "$Id: ascl-ob-persistant.adb,v 1.7 1999/08/21 14:40:31 erdmann Exp erdmann $";
   Module  : constant String := "ASCL.OB.Peristant";

   ---====================================================================---
   ---===                O B J  E C T     D A T A                      ===---
   ---====================================================================---

   --|
   --| Object table
   --|
   type OT_Record;
   type OT_Access is access OT_Record;

   type OT_Record is record
         id       : Object_ID      := 0;
         pool     : Pool_ID        := Pool_ID_Null;
         lt       : OT_Access      := null;
         gt       : OT_Access      := null;

         Offset_Valid : Boolean        := False;
         offset       : Positive_Count := 1;
      end record;

   procedure Free is new Unchecked_Deallocation( OT_Record, OT_Access);

   type External_OT_Record is record
          id      : Object_ID      := 0;
          offset  : Positive_Count := 1;
       end record;

   --|
   --| Pool table
   --|
   type Pool_Record is record
         In_Use     : Boolean    := False;
         Objects    : OT_Access  := null;
         OT_File    : File_Type;
         OT_End     : Positive_Count;
         Data_File  : File_Type;
         Data_End   : Positive_Count;

         Debug      : Debugging_Support.Handle := null;
      end record;

   subtype Valid_Pool_ID is Pool_ID range 1..Pool_ID'Last;

   type Pool_Table is array( Valid_Pool_ID ) of Pool_Record;

   PT : Pool_Table;

   --|
   --| Class table
   --|
   type Class_Record is record
         name  : Tag;
         pool  : Pool_ID := Pool_ID_Null;
         debug : Debugging_Support.Handle := null;
      end record;

   type Class_ID is new Integer range 0..1000;
   type Class_Table is array( 1..Class_ID'Last ) of Class_Record;

   CT : Class_Table;


   ---=====================================================================---
   ---===         L O C A L   S U P P O R T   P R O C E D U R E S       ===---
   ---=====================================================================---


   ---********************    P O O L   T A B L E  ************************---

   --|
   --| check if the referenced pool is in use
   --|
   function Used ( id : Pool_ID ) return Boolean is
   begin
      return PT(id).In_Use;
   end Used;

   --|
   --| Allocate a pool and set the stream
   --|
   function Add_To_Pool_Table return Pool_ID is
   begin
      for i in Valid_Pool_ID loop
         if not Used( i ) then
            PT(i).In_Use := True;
            return i;
         end if;
      end loop;

      raise Out_Of_Memory;
   end Add_To_Pool_Table;

   --|
   --| Remove an entry from the pool table.
   --|
   procedure Delete_From_Pool_Table( pool : Pool_ID ) is
   begin
      if not Used( pool ) then
         raise Usage_Error;
      end if;
      PT( pool ).In_Use := False;

   end Delete_From_Pool_Table;

   --|
   --| Find the pool id of an given object
   --|
   function Find( this : Object'Class ) return Pool_ID is
   begin
      for i in 1..Class_ID'Last loop
         if CT(i).Name = This'Tag then
            return CT(i).Pool;
         end if;
      end loop;

      return Pool_ID_Null;
   end Find;


   --*****************  C L A S S      T A B L E      ***********************--

   --|
   --| Check if an entry is used
   --|
   function Used ( id : Class_ID ) return Boolean is
   begin
      return CT(id).Pool /= Pool_ID_NUll;
   end Used;

   --|
   --| Add a name and pol to the class table.
   --|
   function Add_To_Class_Table( pool : in Pool_ID; name : Tag ) return Class_ID is
   begin
      for i in 1..Class_ID'Last loop
         if not Used( i ) then
            CT(i).Pool := pool;
            CT(i).Name := name;
            return i;
         end if;
      end loop;

      raise Out_Of_Memory;

   end Add_To_Class_Table;

   --|
   --| Delete all references to a cartain pool from the class
   --| table.
   --|
   procedure Delete_From_Class_Table( pool : in Pool_ID ) is
   begin
      if not Used( pool ) then
         raise Usage_Error;
      end if;

      for i in 1..Class_ID'Last loop
         if Used(i) and then CT(i).Pool = pool then
            CT(i).Pool := Pool_ID_Null;
         end if;
      end loop;

   end Delete_From_Class_Table;

   ---***************** O B J E C T   T A B L E ***************************---

   --|
   --| Return true if the offset is a valid one, which
   --| means the object is allready allocated in the File.
   --|
   function Is_Allocated ( o : in OT_Access ) return Boolean is
   begin
      return o.Offset_Valid;
   end Is_Allocated;

   --|
   --| Insert an object in the object tree. If the object is already
   --| in the data will not be overwritten and the function will
   --| return the pointer to the allready stored item.
   --|
   function Add_To_Object_Table( pool : in Pool_ID ;
                                 item : in OT_Access ) return OT_Access is

      Result   : OT_Access := item;
      debug    : Debugging_Support.Handle renames PT( pool ).debug;

      procedure Insert( this : OT_Access ) is
      begin
         if item.id < this.id then
            if item.lt /= null then
               Insert( item.lt );
            else
               this.lt := item;
            end if;
            return;
         end if;

         if item.id > this.id then
            if item.gt /= null then
               Insert( item.gt );
            else
               this.gt := item;
            end if;
            return;
         end if;

         Result := this;
      end Insert;

   begin
      Enter( debug, Module & ".Add_To_Object_Table " &
                    " Pool=" & Pool_ID'Image( pool ) &
                    " Object=" & Object_ID'Image( item.id ) );

      if PT(pool).Objects /= null then
         Insert( PT(pool).Objects );
      else
         PT(pool).Objects := item;
      end if;

      Leave( debug, "");
      return Result;

   exception
      when The_Error : Others =>
         raise;
   end Add_To_Object_Table;

   --|
   --| Find the object entry in the object tree. If it is
   --| not in the tree return null pointer.
   --|
   function Find( root : OT_Access; id : Object_ID ) return OT_Access is
   begin
      if root.id = id then
         return root;
      end if;

      if root.lt /= null and then root.id > id then
         return Find( root.lt, id );
      end if;
      if root.gt /= null and then root.id < id then
         return Find( root.gt, id );
      end if;

      return null;
   end Find;

   --|
   --| Find object in the object data table.
   --|
   function Find ( this : Object'Class ) return OT_Access is
      pool : Pool_ID := Find( this );
   begin
      if pool /= Pool_ID_Null then
         return Find( PT(pool).Objects, this.id );
      end if;

      return null;
   end Find;

   --|
   --|  Write the contensts of the object table into a
   --|  file.
   --|  A local procedure is used to walk the object tree
   --|  an to store the objects
   --|
   --|
   procedure Write_Object_Table( pool : in Pool_ID ) is

      Output   : Stream_Access := null;
      File     : File_Type renames PT( pool ).OT_File;
      External : External_OT_Record;
      debug    : Debugging_Support.Handle renames PT(Pool).Debug;

      procedure Write_Element( item : in OT_Access ) is
      begin
         if item.lt /= null then
            Write_Element( item.lt );
         end if;

         External.Id     := item.id;
         External.Offset := item.Offset;
         External_OT_Record'Write(Output, External );

         if item.gt /= null then
            Write_Element( item.gt );
         end if;

      exception
         when The_Error : Others =>
            Error( debug,
                  "*** Exception " & Exception_Name( The_Error ) &
                  " in " & Module & ".Write_Object_Table.Write_Element " &
                  " id = " & Object_ID'Image( item.id ) );
            raise;
      end Write_Element;

   begin
       Enter( debug, Module & ".Write_Object_Table " &
                     "Pool =" & Pool_ID'Image( pool ) );

      if not Used( pool ) then
         raise Usage_Error;
      end if;

      Reset( File, Mode => Out_File );

      Output := Stream( File );

      if PT( pool ).Objects /= null then
         Write_Element( PT( pool ).Objects );
      end if;

      Flush( File );

      Leave( debug, "" );

   end Write_Object_Table;

   --|
   --| Read the object table from the given file.
   --|
   procedure Read_Object_Table( pool : in Pool_ID ) is
      Input    : Stream_Access := null;
      File     : File_Type renames PT( pool ).OT_File;
      debug    : Debugging_Support.Handle renames PT(Pool).Debug;
   begin
      Enter( debug, Module &  ".Read_Object_Table");
      if not Used( pool ) then
         raise Usage_Error;
      end if;

      if not Is_Open( File ) then
         raise Usage_Error;
      end if;

      Reset( File, Mode => In_File );

      Input := Stream( File );

      while not End_Of_File( File ) loop
         declare
            OT_Entry : OT_Access := null;
            External : External_OT_Record;
         begin

            External_OT_Record'Read( Input, External );

            PT( pool ).OT_End := Index( File );

            OT_Entry := new OT_Record;
            OT_Entry.id     := External.Id;

            OT_Entry.Offset       := External.Offset;
            OT_Entry.Offset_Valid := True;

            OT_Entry := Add_To_Object_Table( pool, OT_Entry );
         end;
      end loop;

      Leave( debug, "" );

   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Read_Object_Table" );
         return;

   end Read_Object_Table;

   --|
   --| Save the object data into the object data file
   --|
   procedure Save_Object( this : in out Object'Class ) is
      OT_Entry : OT_Access := null;
      Pool     : Pool_ID   := Find( this );
      File     : File_Type renames PT(pool).Data_File;
      S        : Stream_Access := Stream( File );
      debug    : Debugging_Support.Handle renames PT(Pool).Debug;
   begin
      Enter( debug, Module & ".Save_Object Object="
                           & Object_ID'Image(this.id) );

      OT_Entry := Find( this );
      if OT_Entry = null then
         raise Usage_Error;
      end if;

      Set_Mode( File, Out_File );
      Set_Index( File, OT_Entry.Offset);
      Write( this, S );
      Flush( File );

      Leave( debug, "" );
   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Save_Object" );
         raise;
   end Save_Object;

   --|
   --| Create the space for a new object in the object
   --| data storage.
   --|
   procedure Make_New_Object_Data( this : in out Object'Class ) is
      OT_Entry : OT_Access := null;
      Pool     : Pool_ID   := Find( this );
      File     : File_Type renames PT(Pool).Data_File;
      S        : Stream_Access := Stream( File );
      debug    : Debugging_Support.Handle renames PT(Pool).Debug;
   begin
      Enter( debug, Module & ".Make_New_Object_Data Object="
                             & Object_ID'Image(this.id) );

      OT_Entry := Find( this );
      if OT_Entry = null then
         raise Usage_Error;
      end if;

      Set_Index( File, PT(Pool).Data_End );
      Set_Mode( File, Append_File );

      OT_Entry.Offset := Index( File );
      Write( this, S );
      OT_Entry.Offset_Valid := True;

      PT(Pool).Data_End := Index( File );
      Flush(File);

      Leave( debug, "" );
   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Make_New_Object_Data" );

   end Make_New_Object_Data;

   --|
   --| Restore the Object from the data file.
   --|
   procedure Restore_Object( this : in out Object'Class ) is
      OT_Entry : OT_Access := null;
      Pool     : Pool_ID   := Find( this );
      File     : File_Type renames PT(pool).Data_File;
      S        : Stream_Access := Stream( File );
      debug    : Debugging_Support.Handle renames PT(Pool).Debug;
   begin
      Enter( debug, Module & ".Restore_Object");
      OT_Entry := Find( this );

      Set_Mode( File, In_File );
      Set_Index( File, OT_Entry.Offset);
      Read( this, S );

      Leave( debug, "" );

   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Resore_Object" );

   end Restore_Object;

   --|
   --| This procedure is called upon every exception raised in this
   --| component. It may be used to trace back the cause of an
   --| exception, or to print out relevant information about the
   --| instance where the problem did occure.
   --|
   procedure Error( this     : in Object'Class;
                    theError : in Exception_Id ;
                    info     : in String )
   is
      OT_Entry : OT_Access := Find( this );
      Pool     : Pool_ID   := Find( this );
      debug    : Debugging_Support.Handle renames PT( Pool ).Debug;
   begin

      if OT_Entry /= null then
         Error( debug, Module & Info );
      end if;

      Raise_Exception( theError, Module & info );
   end Error;

   ---======================================================================---
   ---===             C O M P O  N E N T    I N T E R F A C E            ===---
   ---======================================================================---

   --|
   --| Purpose:
   --|   Initialize the instance and assign the debugging support.
   --|   Identifiy the resposible pool and load the data from
   --|   the open stream.
   --|
   --|
   procedure Initialize( this : in out Object ) is
      OT_Entry : OT_Access := null;
      Pool     : Pool_ID   := Find( this );
      Debug    : Debugging_Support.Handle renames PT( Pool ).Debug;
   begin
      Enter( debug, Module & ".Initialize " &
                             " Object=" & Object_ID'Image( this.id ) &
                             " Pool=" & Pool_ID'Image(Pool) );

      if Pool = Pool_ID_Null then
         raise Usage_Error;
      end if;

      -- create an instance or load the instance data
      OT_Entry       := new OT_Record;
      OT_Entry.id    := this.id;
      OT_Entry.Pool  := Pool;

      OT_Entry := Add_To_Object_Table( Pool, OT_Entry );

      -- read the data or write an empty instance into the file
      if Is_Allocated( OT_Entry  ) then
         Restore_Object( this );
      else
         Make_New_Object_Data( this );
      end if;

      Leave( Debug, "" );
   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Initialize");
   end Initialize;

   --|
   --| Purpose:
   --|   Finalize the persitant object by writing back the
   --|   Object data.
   --|
   --|
   procedure Finalize( this : in out Object ) is
      OT_Entry : OT_Access := Find( this );
      Pool     : Pool_ID   := Find( this );
      debug    : Debugging_Support.Handle renames PT( Pool ).Debug;
   begin
      Enter( debug, Module & ".Finalize Object=" & Object_ID'Image(this.id) );

      if OT_Entry = null then
         raise Not_Initialized;
      end if;

      Save_Object( this );

      Leave( debug, "");
   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Finalize");
   end Finalize;

   ---=====================================================================---
   ---===           A T T R I B U T E    F U N C T I O N S              ===---
   ---=====================================================================---

   --|
   --| Purpose:
   --|    Return the identifier od the Object
   --|
   function Id( this : in Object'Class ) return Object_ID is
   begin
      return this.id;
   end Id;

   ---=====================================================================---
   ---===                        M E T H O D S                          ===---
   ---=====================================================================---

   --|
   --| Purpose:
   --|    Open a new pool by opening the named file. If it exists, the
   --|    object table will be read in
   --|
   --|
   function Open( name  : in String;
                  debug : in Debugging_Support.Handle := null ) return Pool_ID is
      Result   : Pool_ID := Pool_ID_Null;

      --
      -- open or create the given file
      --
      procedure Open_Or_Create_File( name : in String; file : out File_Type ) is
      begin
         Enter( debug, Module & ".Open_Or_Create_File" );
         begin
            Open( Name => name,
                  Mode => In_File,
                  File => File );
         exception
            when Name_Error =>
               Create(
                  Name => name,
                  Mode => In_File,
                  File => File );
               Set_Mode( File, In_File );

            when The_Error : Others =>
               raise;
         end;

         Leave( debug, "" );

      end Open_Or_Create_File;

   begin
      Enter( debug, Module & ".Open");

      Result := Add_To_Pool_Table;
      if Result = Pool_ID_Null then
         raise Out_Of_Memory;
      end if;

      PT( Result ).debug     := debug;

      Open_Or_Create_File( name & ".ot", PT( Result ).OT_File);
      Open_Or_Create_File( name & ".ob", PT( Result ).Data_File);

      Read_Object_Table( Result );

      Leave( debug, "");
      return Result;

   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Open" );

         raise;
   end Open;

   --|
   --| Purpose:
   --|    Close the object pool.
   --|
   --| Precondition
   --|    Pool id is allocated
   --|
   procedure Close( pool : in Pool_ID ) is
      Debug : Debugging_Support.Handle;
   begin
      if not Used( pool ) then
         raise Usage_Error;
      end if;

      Debug := PT(pool).Debug;
      Enter( debug, Module & ".Close");

      Write_Object_Table( pool );
      Flush( PT(pool).OT_File );
      Close( PT(pool).OT_File );

      Flush( PT(pool).Data_File );
      Close( PT(pool).Data_File );

      Delete_From_Class_Table( pool );
      Delete_From_Pool_Table( pool );

      Leave( debug, "");
   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Close" );
         raise;
   end Close;

   --|
   --| Purpose:
   --|    Add a name an pool id into the class table.
   --|
   procedure Add( pool   : in Pool_ID;
                  name   : in Tag;
                  debug  : in Debugging_Support.Handle := null ) is
      Id : Class_ID;
   begin
      Enter( debug, Module & ".Add " & Expanded_Name( name ) );

      if not Used( pool ) then
         raise Usage_Error;
      end if;

      Id := Add_To_Class_Table( pool, name );
      CT(Id).Debug := debug;

      Leave( debug, "" );
   exception
      when The_Error : Others =>
         Error( debug,
             "*** Exception " & Exception_Name( The_Error ) &
             " in " & Module & ".Add" );

   end Add;

end ASCL.OB.Persistant;
