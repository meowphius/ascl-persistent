--|
--| Filename        : $Source: /home/erdmann/ASCL/Template/RCS/template.adb,v $
--| Description     : Component Template
--| Author          : Michael Erdmann
--| Created On      : 25.3.1999
--| Last Modified By: $Author: erdmann $
--| Last Modified On: $Date: 1999/08/01 10:56:43 $
--| Status          : $State: Exp $
--|
--| Functional Description
--| ======================
--|
--|
--| Restrictions
--| ============
--|
--| References
--| ==========
--|
--|
--| History
--| =======
--| $Log: template.adb,v $
--| Revision 1.2  1999/08/01 10:56:43  erdmann
--| Debugging support added
--|
--| Revision 1.1  1999/07/18 15:35:30  erdmann
--| No comments
--|

--* Ada
with Ada.Exceptions;               use Ada.Exceptions;
with Unchecked_Deallocation;

with ASCL.Debugging_Support;  
use  ASCL;

package body ASCL.OB.Persistant is

   Version : constant String := "$Id: template.adb,v 1.2 1999/08/01 10:56:43 erdmann Exp erdmann $";
   Module  : constant String := "ASCL.OB.Peristant";

   ---====================================================================---
   ---===                O B J  E C T     D A T A                      ===---
   ---====================================================================---

   --|
   --| This is the instance of the component internal data.
   --|
   type OT_Record is record
         debug   : Debugging_Support.Handle := null;
      end record;

   type OT_Access is access OT_Record;
   procedure Free is new Unchecked_Deallocation( OT_Record, OT_Access);

   type Object_Table is array( 1..Object_ID'Last ) of OT_Access;



   OT : Object_Table := (Others => null);

   ---=====================================================================---
   ---===         L O C A L   S U P P O R T   P R O C E D U R E S       ===---
   ---=====================================================================---

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
   begin

      Raise_Exception( theError,  "ASCL.OB.Persiantant.adb" & info );
   end Error;

   ---======================================================================---
   ---===             C O M P O  N E N T    I N T E R F A C E            ===---
   ---======================================================================---

   --|
   --| Purpose:
   --|   Initialize the instance and assign thedebugging support. 
   --|
   --| Pre-Conditions:
   --|    Object shall not be initialized.
   --|
   --| Post-Conditions:
   --|
   --|
   procedure Initialize( this  : in out Object'Class;
                         debug : in Debugging_Support.Handle := null )  
   is
      OT_Entry : OT_Access := null;
   begin
      if OT(this.id) /= null then
         raise Usage_Error;
      end if;
      OT_Entry := new OT_Record;

      OT_Entry.debug := debug;

      OT( this.id) := OT_Entry;

   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Initialize");
   end Initialize;

   --|
   --| Purpose:
   --|
   --| Pre-Conditions:
   --|
   --| Post-Conditions:
   --|
   --|
   procedure Finalize( this : in out Object'Class ) is
   begin
      if OT(this.id) = null then
         raise Not_Initialized;
      end if;


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
   --| Pre-Conditions:
   --|    The Object has to be initialized
   --|
   --| Post-Conditions:
   --|
   --|
   function Id( this : in Object'Class ) return Object_ID is
   begin
      if OT(this.id) = null then
         raise Not_Initialized;
      end if;

      return this.id;

   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Id");
         raise;

   end Id;


   ---=====================================================================---
   ---===                        M E T H O D S                          ===---
   ---=====================================================================---

   procedure Free( this : in out Handle ) is
      procedure OB_Free is 
         new Unchecked_Deallocation( Object'Class, Handle);
   begin
      OB_Free( this );

      this := null;

   exception
      when The_Error : Others =>
         raise;         
   end Free;

end ASCL.OB.Persistant;

