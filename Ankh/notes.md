so I think what's happening is that assigning one variable to another does not work well for some reason.
So the problem is with the val value, let's trace how this changes.
So first we get LocalVariableAST, we codegen that storing the value in g_named_values. Then we assign y=x, which means x is parsed as VaraibleAST.
I think the problem is that we create the load in one function but then call the loaded value in another.
