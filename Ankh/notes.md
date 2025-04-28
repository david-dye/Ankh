so I think what's happening is that assigning one variable to another does not work well for some reason.
So the problem is with the val value, let's trace how this changes.
So first we get LocalVariableAST, we codegen that storing the value in g_named_values. Then we assign y=x, which means x is parsed as VaraibleAST.
I think the problem is that we create the load in one function but then call the loaded value in another.

Apr 27: 
Trying to handle the problem with optimization, let's try to find when the problem occurs.
I know it occurs for while loops, does it occur for if statements?
also check the tutorial.
Alright, so the problem is only with while.
Potential leads:
    - look at llvm IR
    - look at tutorial
    - look at the code for while
Note that the thing breaks when adding no_opt, meaning that it works when optimized but not when not optimized. This may indicate that the implementation may have some problem.
LLVM differences:
    - there's an alloca in the case of non optimized code
Now that I think about it, the problem is probably related to allocas, maybe it's about the allocas isnide the while not being accessible outside or something?
    - Notably, for test_while_simple, x2 is trying to load the value from x1.
Another possibility is something like the allocas being flushed when generting the object file?
One sus thing is that he loop part, which uses cond_expr->codegen(),X, actually this is not true, but why do we need to go back to the condition to figure out what to return.
One potential thing is the GetInsertBlock() thing.
    - changing this to body_bb, cond_bb seems to not fix the problem
The tutorial mentions remembering the block before the loop, but this doesn't seem to be done in the while expr codegen.
There's something about where the phinode is loading from.
