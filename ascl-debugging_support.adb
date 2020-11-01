--|
--| Filename        : $Source: /home/erdmann/ASCL/Library/RCS/ascl-debugging_support.adb,v $
--| Description     : Base class for debugging support
--| Author          : Michael Erdmann
--| Created On      : 25.3.1999
--| Last Modified By: $Author: erdmann $
--| Last Modified On: $Date: 1999/08/16 18:20:04 $
--| Status          : $State: Exp $
--|
--| Functional Description
--| ======================
--|
--| This module provide the infrstructure of the debugging/tracing
--| within an ada library. The procedure output which has to be
--| supplied by the extension is the gateway for the debuging
--| information. It has to be adopted to the environment which
--| is used.
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
--| $Log: ascl-debugging_support.adb,v $
--| Revision 1.3  1999/08/16 18:20:04  erdmann
--| Test complete
--|
--| Revision 1.2  1999/08/08 19:22:36  erdmann
--| fixed bugs in: Back_Trace, Error
--|
--| Revision 1.1  1999/08/01 19:15:38  erdmann
--| Specification complete and compiled
--|

--* Ada
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Text_IO;                  use Ada.Text_IO;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with Unchecked_Deallocation;

--* ASCL Support

package body ASCL.Debugging_Support  is

   Version : constant String := "$Id: ascl-debugging_support.adb,v 1.3 1999/08/16 18:20:04 erdmann Exp erdmann $";

   Module  : constant String := "ASCL.Debugging_Support";
   ---====================================================================---
   ---===                O B J  E C T     D A T A                      ===---
   ---====================================================================---

   type Trace_Element;
   type Trace_Element_Access is access Trace_Element;

   type Trace_Element is record
         next : Trace_Element_Access := null;
         item : Unbounded_String;
      end record;

   --|
   --| This is the instance of the component internal data.
   --|
   type Object_Data is record
         level      : Debug_Level := Quiet;
         call_level : Natural     := 0;
         first      : Trace_Element_Access := null;
      end record;

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
      data : Object_Data_Access := this.data;
   begin
      Put_Line("*** Exception " &
                Exception_Name( theError ) &
                " in " &
               info );

      Raise_Exception( theError,  Module & ":" & info );
   end Error;

   --|
   --| Trace into some makred section.
   --|
   procedure Trace_Into( data : Object_Data_Access; text : in String ) is
      element : Trace_Element_Access := new Trace_Element;
   begin
      element.item := To_Unbounded_String( text );

      if data.first = null then
         data.first := element;
      else
         element.next := data.first;
         data.first := element;
      end if;

   exception
      when The_Error : Others =>
         raise;
   end Trace_Into;

   --|
   --| Return from the last trace point.
   --|
   procedure Return_From( data : in Object_Data_Access ) is
      element : Trace_Element_Access := null;
   begin
      if data.first /= null then
         element := data.first;
         data.first := element.next;
      end if;

   exception
      when The_Error : Others =>
         raise;
   end Return_From;

   --|
   --| generate a back trace.
   --|
   procedure Back_Trace( this : in Handle ) is
      data    : Object_Data_Access := this.data;
      element : Trace_Element_Access := data.first;
   begin
      while element /= null loop
         Output( this.all, To_String( element.item ) );
         element := element.next;
      end loop;

   exception
      when The_Error : Others =>
         Error( this.all, Exception_Identity( The_Error ), ".Back_Trace" );
         raise;
   end Back_Trace;

   ---======================================================================---
   ---===             C O M P O  N E N T    I N T E R F A C E            ===---
   ---======================================================================---

   --|
   --| Intialize the Object data and call the Initalization
   --| procedure of the extention.
   --|
   procedure Initialize( this  : in out Object'Class ) is
      data             : Object_Data_Access renames this.data;
   begin
      if data /= null then
         raise Usage_Error;
      end if;

      data := new Object_Data;
      if data = null then
         raise Out_Of_Memory;
      end if;

   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Initialize");
   end Initialize;

   --|
   --| Finalize the instance by releasing the object data
   --| finalizing the extention.
   --|
   procedure Finalize( this : in out Object'Class ) is
      data   : Object_Data_Access renames this.data;

      procedure Free is
            new Unchecked_Deallocation( Object_Data, Object_Data_Access);
   begin
      if data = null then
         return;
      end if;

      Free( data );
      this.data := null;

   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Finalize");
   end Finalize;

   ---=====================================================================---
   ---===           A T T R I B U T E    F U N C T I O N S              ===---
   ---=====================================================================---

   --|
   --| This function reads an attribute
   --|
   function Get( this : in Object'Class ) return Debug_Level is
      data : Object_Data_Access := this.data;
   begin
      if data = null then
         raise Not_Initialized;
      end if;

      return data.level;

   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Get Debug_Level");
         raise;
   end Get;

   --|
   --| Set the debuging level.
   --|
   procedure Set( this : in out Object'Class; level : in Debug_Level ) is
      data : Object_Data_Access := this.data;
   begin
      if data = null then
         raise Not_Initialized;
      end if;

      data.level := level;

   exception
      when The_Error : Others =>
         Error( this, Exception_Identity( The_Error ), ".Set Debug_Level" );
         raise;
   end Set;

   ---=====================================================================---
   ---===                        M E T H O D S                          ===---
   ---=====================================================================---

   --|
   --| Purpose:
   --|    Display debugging information upon entering the context
   --|    of a procedure, block etc.... The trace information is
   --|    stored in the trace list.
   --|
   --| Pre-Conditions:
   --|    Instance has been initialized.
   --|
   --| Post-Conditions:
   --|    not applicable
   --|
   procedure Enter( this : in Handle; text : in String ) is
      data       : Object_Data_Access   := null;
      element    : Trace_Element_Access := null;
   begin
      if this = null then
         return;
      end if;

      if this.data = null then
         raise Not_Initialized;
      else
         data := this.data;
      end if;

      data.call_level := data.call_level + 1;

      case data.level is
         when Logic | Verbose =>
            Output( this.all, data.call_level * "   " & "-> " & text );
         when Others =>
            null;
      end case;

      Trace_Into( data, text );

   exception
      when The_Error : Others =>
         Error( this.all, Exception_Identity( The_Error ), ".Enter" );
         raise;
   end Enter;

   --|
   --| Purpose:
   --|    Display debugging information upon leaving the context
   --|    of a procedure, block etc...
   --|
   --| Pre-Conditions:
   --|    Instance has been initialized.
   --|
   --| Post-Conditions:
   --|    not applicable
   --|
   procedure Leave( this : in Handle; text : in String ) is
      data : Object_Data_Access := null;
   begin
      if this = null then
         return;
      end if;

      if this.data = null then
         raise Not_Initialized;
      else
         data := this.data;
      end if;


      case data.level is
         when Logic | Verbose  =>
            Output( this.all,
                   data.call_level * "   " &
                   "<- " & To_String( data.first.item ) & " " & text );
          when Others =>
            null;
      end case;

      if data.call_level /= 0 then
         data.call_level := data.call_level - 1;
      end if;

      Return_From( data );

   exception
      when The_Error : Others =>
         Error( this.all, Exception_Identity( The_Error ), ".Enter" );
         raise;
   end Leave;

   --|
   --| Purpose:
   --|    Display debugging information about an error occured in
   --|    the application.
   --|
   --| Pre-Conditions:
   --|    Instance has been initialized.
   --|
   --| Post-Conditions:
   --|    not applicable
   --|
   procedure Error( this : in Handle; text : in String ) is
      data : Object_Data_Access := null;
   begin
      if this = null then
         return;
      end if;

      if this.data = null then
         raise Not_Initialized;
      else
         data := this.data;
      end if;

      Output( this.all, text );
      Output( this.all, "Backtrace:");

      Back_Trace( this );

   exception
      when The_Error : Others =>
         Error( this.all, Exception_Identity( The_Error ), ".Enter" );
         raise;
   end Error;


   --|
   --| Purpose:
   --|    Display debugging information
   --|
   --| Pre-Conditions:
   --|    Instance has been initialized.
   --|
   --| Post-Conditions:
   --|    not applicable
   --|
   procedure Trace( this : in Handle; text : in String ) is
      data : Object_Data_Access := null;
   begin
      if this = null then
         return;
      else
         data := this.data;
      end if;

      case data.level is
         when Verbose | Logic =>
            Output( this.all, 
                    data.call_level * "   " & "  " &  text );
         when Others =>
            null;
      end case;

   end Trace;

end ASCL.Debugging_Support;
