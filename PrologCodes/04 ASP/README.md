### Part 1

Block program<br>

initially(handempty).<br>
initially(clear(a)).<br>
initially(clear(c)).<br>
initially(ontable(c)).<br>
initially(ontable(b)).<br>
initially(on(a,b)).<br>
finally(on(a,c)).<br>
finally(on(c,b)).<br>
finally(ontable(b)).<br>

Run Command:<br>
lparse blocks2.txt | smodels 0

Block Program 2<br>


initially(handempty).<br>
initially(clear(a)).<br>
initially(clear(d)).<br>
initially(clear(e)).<br>

initially(ontable(c)).<br>
initially(ontable(e)).<br>
initially(ontable(d)).<br>
initially(on(b,c)).<br>
initially(on(a,b)).<br>


%finally(ontable(b)).<br>
finally(on(d,b)).<br>
%finally(ontable(e)).<br>
finally(on(c,e)).<br>
finally(ontable(a)).<br>

Run Command:<br>
lparse blocks3.txt | smodels 0

