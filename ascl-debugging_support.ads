--|  
--| Filename        : $Source: /home/erdmann/ASCL/Library/RCS/ascl-debugging_support.ads,v $
--| Description     : Debuging object
--| Author          : Michael Erdmann
--| Created On      : 25.3.1999
--| Last Modified By: $Author: erdmann $
--| Last Modified On: $Date: 1999/08/16 18:17:48 $
--| Status          : $State: Exp $
--|
--|
--| Functional Description
--| ======================
--| This packages provides a so called debugging object. An application
--| has to provide the output procedure.
--| The following procedures are provided:
--|
--| Enter/Leave - Enter/Leave a context
--| Trace       - Output trace information
--|
--|
--|
--| Component Data
--| ==============
--| Debugging Level  - Set/Get the following values are possible:
--|
--|                    Quiet   - Not trace at all
--|                    Logic   - Only the call trace is shown
--|                    Verbose - Every trace information is shown
--|
--|
--| Error Handling
--| ==============
--|
--|
--| Extension
--| =========
--|
--| Restrictions
--| ============
--| Tasking: no
--| Y2K    : yes
--|
--|
--| History
--| =======
--| $Log: ascl-debugging_support.ads,v $
--| Revision 1.2  1999/08/16 18:17:48  erdmann
--| Documentation added, Test complete
--|
--| Revision 1.1  1999/08/01 19:14:59  erdmann
--| Specification complete and compiled
--|
--|
package ASCL.Debugging_Support is
   
   ---=====================================================================---
   ---===             C O M P O N E N T   I N T E R F A C E             ===---
   ---=====================================================================---

   type Object is abstract tagged private;
   type Handle is access all Object'Class;

   procedure Initialize( this : in out Object'Class );
   procedure Finalize( this : in out Object'Class );

   ---=====================================================================---
   ---===                  A T T R I B U T E S                       ===---
   ---=====================================================================---
   type Debug_Level is ( Quiet, Logic, Verbose );

   procedure Set( this : in out Object'Class; level : in Debug_Level );
   function  Get( this : in Object'Class ) return Debug_Level;

   ---=====================================================================---
   ---===                     M E T H O D S                             ===---
   ---=====================================================================---

   ---------------------------------------------------------------------------
   --| Description    : This method indicates that an error has occured. 
   --| Preconditions  : Instance has been initialized.
   --| Postconditions : -
   --| Exceptions     : -
   --| Note           : Ouput is used to output the error message.
   ---------------------------------------------------------------------------
   procedure Error( this : in Handle; text : in String );

   ---------------------------------------------------------------------------
   --| Description    : Output trace information 
   --| Preconditions  : Instance has been initialized and trace level
   --|                  is Verbose.
   --| Postconditions : -
   --| Exceptions     : -
   --| Note           : Ouput is used to output the error message.
   ---------------------------------------------------------------------------
   procedure Trace( this : in Handle; text : in String );

   ---------------------------------------------------------------------------
   --| Description    : Enter a logical block procedure or function 
   --| Preconditions  : Instance has been initialized and trace level
   --|                  is Verbose or Logic.
   --| Postconditions : -
   --| Exceptions     : -
   --| Note           : Output is used for trace information
   ---------------------------------------------------------------------------
   procedure Enter( this : in Handle; text : in String );

   ---------------------------------------------------------------------------
   --| Description    : Leave a logical block, procedure o function
   --| Preconditions  : Instance has been initialized an trace level
   --|                  is Verbose or Logic.
   --| Postconditions :
   --| Exceptions     :
   --| Note           : Output is used for trace information
   ---------------------------------------------------------------------------
   procedure Leave( this : in Handle; text : in String );

   ---=====================================================================---
   ---===                   E X T E N S I O N                           ===---
   ---=====================================================================---  

   procedure Output( this : in out Object; text : in String ) is abstract;

   ---=====================================================================---
private
   type Object_Data;
   type Object_Data_Access is access Object_Data;

   type Object is abstract tagged record
         data : Object_Data_Access := null;
      end record; 

end ASCL.Debugging_Support;


