with Tropos;

package Carthage.Galaxy.Configure is

   procedure Configure_Gates
     (Gate_Config : Tropos.Configuration);

   procedure Configure_Positions
     (Position_Config : Tropos.Configuration);

   procedure Import_Gate
     (From, To : Positive);

end Carthage.Galaxy.Configure;
