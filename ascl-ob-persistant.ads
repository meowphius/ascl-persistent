--|  
--| Filename        : $Source: /home/erdmann/ASCL/Library/RCS/ascl-ob-persistant.ads,v $
--| Description     : Persistant Objects Base Class
--| Author          : Michael Erdmann
--| Created On      : 25.3.1999
--| Last Modified By: $Author: erdmann $
--| Last Modified On: $Date: 1999/08/21 15:06:34 $
--| Status          : $State: Exp $
--|
--| Functional Description
--| ======================
--| Persistant objects are restored from a file upon instanciation
--| and automaticaly saved into the same file during finalization
--| of the instance.
--| In order to read and store an object from/into the file 
--| the procedures Read and Write have to be supplied by the implementation
--| of the object.
--| The file which contains the data is opend by the Open call. It 
--| returns a so called pool identifier. 
--| Each derived type has to be assgined to such a pool by using
--| the Add method.
--| 
--| Example:
--|    The example below shos the life cycle of a persistant object
--|    assuming, that Test.Persistant class has been drived from the 
--|    persitant object class which supports the attribute functions
--|    Value.
--| 
--|
--| procedure Main is
--|   Pool : Pool_ID;
--| begin
--|
--|   Pool := Open( "Objects", D);    -- Open the object sore
--|                                   -- Add an object clas to the pool
--|   Add( Pool, Test.Persistant.Object'Tag );
--|
--|   declare
--|      O   : Test.Persistant.Object(1);  -- Object 1
--|      P   : Test.Persistant.Object(2);  -- OBject 2
--|   begin
--|      if Value( O ) = 0 then
--|         Value( O, 1 );
--|      else
--|         Trace( D, "Value = " & Integer'Image( Value(O) ) );
--|      end if;
--|   end;
--|
--|   Close( Pool );                 -- close the storage
--|
--| End Main;
--|
--| Component Data
--| ==============
--| Object_ID      - Object identifier supplied by the application.
--| 
--|
--| Error Handling
--| ==============
--|
--| Extension
--| =========
--|
--| Restrictions
--| ============
--| Tasking: yes/no
--| Y2K    :
--|
--| References
--| ==========
--| #$-DOCUMENT: Class specification -$#
--|
--| History
--| =======
--| $Log: ascl-ob-persistant.ads,v $
--| Revision 1.5  1999/08/21 15:06:34  erdmann
--| Test finished
--|
--| Revision 1.4  1999/08/14 12:26:17  erdmann
--| No comments
--|
--| Revision 1.3  1999/08/08 16:22:00  erdmann
--| Test Version
--|
--| Revision 1.2  1999/08/08 09:51:55  erdmann
--| intermidiate version, dont use
--|
--| Revision 1.1  1999/08/04 20:05:27  erdmann
--| compiled & not tested
--|
--|
with Ada.Finalization;            use Ada.Finalization;
with Ada.Tags;                    use Ada.Tags;
with Ada.Streams.Stream_IO;       use Ada.Streams.Stream_IO;
with Ada.Finalization;            use Ada.Finalization;

with ASCL.Debugging_Support; 
use  ASCL;   


package ASCL.OB.Persistant is
   
   ---=====================================================================---
   ---===             C O M P O N E N T   I N T E R F A C E             ===---
   ---=====================================================================---
   type Object_ID is new Integer;

   type Object( this : Object_ID ) is abstract new Controlled with private;
   type Handle is access Object'Class;

   type Pool_ID is private;
   Pool_ID_Null : constant Pool_ID;


   ---=====================================================================---
   ---===                  A T T R I B U T E S                       ===---
   ---=====================================================================---

   ---------------------------------------------------------------------------
   --| Description    : 
   --|      Query the Object identifier
   --| Preconditions  :
   --|      none
   --| Postconditions :
   --|      none
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   function Id( this : in Object'Class ) return Object_ID;

   ---=====================================================================---
   ---===                     M E T H O D S                             ===---
   ---=====================================================================---

   ---------------------------------------------------------------------------
   --| Description    :
   --|     Open a new object pool. This will create or open
   --|     the following files <name>.ot and <name>.ob. The function 
   --|     returns a pool identifier which has to be used later 
   --|     on in all pool related commands.
   --| Preconditions  :
   --|     The name has to be a valid file name.
   --| Postconditions :
   --| Exceptions     :
   --|     Out_of_Memory - Pool table full.
   --| Note           :
   ---------------------------------------------------------------------------
   function Open( name  : in String;
                  debug : in Debugging_Support.Handle := null ) return Pool_ID;

   ---------------------------------------------------------------------------
   --| Description    :
   --|    Close the named pool
   --| Preconditions  :
   --|    The Pool_ID has to be allocated previously by means of the 
   --|    open call.
   --| Postconditions :
   --| Exceptions     :
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Close( pool : in Pool_ID );

   ---------------------------------------------------------------------------
   --| Description    : 
   --|    Add a class to a storage pool.
   --| Preconditions  :
   --|    The pool identified by the pool id has to be open.
   --| Postconditions :
   --| Exceptions     :
   --|    Out_Of_Memory    - No more classes possible
   --| Note           :
   ---------------------------------------------------------------------------
   procedure Add( pool  : in Pool_ID; 
                  name  : in Tag;
                  debug : in Debugging_Support.Handle := null );

   ---=====================================================================---
   ---===                   E X T E N S I O N                           ===---
   ---=====================================================================---
   procedure Write( this : in out Object; stream : Stream_Access ) is abstract;
   procedure Read(  this : in out Object; stream : Stream_Access ) is abstract;

   ---=====================================================================---
private

   type Object( this : Object_ID ) is abstract new Controlled with record
         id      : Object_ID := this;
      end record; 

   procedure Initialize( this : in out Object );
   procedure Finalize( this : in out Object );

   type Pool_ID is new Integer range 0..1024;

   Pool_ID_Null : constant Pool_ID := 0;

end ASCL.OB.Persistant;


