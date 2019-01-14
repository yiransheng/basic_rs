var N = 0;
var C = 0;
var M = 0;
var B = 0;
var P = 0;
var A = 0;
var W = 0;
var fn$0 = function* () {
var _label;
var $x0;
$x0 = 0;
$x0 = N;
if (( M===1 )) {
_label = 2;
}
else {
_label = 1;
}
$L1: do {
if (_label === 1) {
$x0 = ( $x0-1 );
_label = 2;
break $L1;
}
} while(0)
if (( M===1 )) {
_label = 10;
}
else {
_label = 3;
}
$L4: do {
if (_label === 3) {
if (( N>A )) {
_label = 5;
break $L4;
}
else {
_label = 4;
}
W = 1;
env.printLabel("COMPUTER TAKES");
env.printAdvance3();
env.print(N);
env.printAdvance3();
env.printLabel("AND LOSES.");
env.printNewline();
return;
}
else if (_label === 10) {
if (( N>B )) {
_label = 5;
break $L4;
}
else {
_label = 11;
}
W = 1;
env.printLabel("COMPUTER TAKES");
env.printAdvance3();
env.print(N);
env.printAdvance3();
env.printLabel("AND WINS.");
env.printNewline();
return;
}
} while(0)
P = ( $x0-( C*Math.trunc( ( $x0/C ) ) ) );
if (( !( P<A ) )) {
_label = 7;
}
else {
_label = 6;
}
$L10: do {
if (_label === 6) {
P = A;
_label = 7;
break $L10;
}
} while(0)
if (( !( P>B ) )) {
_label = 9;
}
else {
_label = 8;
}
$L13: do {
if (_label === 8) {
P = B;
_label = 9;
break $L13;
}
} while(0)
N = ( N-P );
env.printLabel("COMPUTER TAKES");
env.printAdvance3();
env.print(P);
env.printAdvance3();
env.printLabel("AND LEAVES");
env.printAdvance3();
env.print(N);
env.printNewline();
W = 0;
return;
}
var fn$1 = function* () {
var _label;
env.printNewline();
env.printLabel("YOUR MOVE ");
env.printAdvance3();
$L1: while (1) {
env.printNewline();
P = yield env.input();
if (( !( P===0 ) )) {
_label = 3;
}
else {
_label = 2;
break $L1;
}
if (( !( P===Math.trunc( P ) ) )) {
_label = 7;
}
else {
_label = 4;
}
$L4: do {
if (_label === 4) {
if (( !( P<A ) )) {
_label = 11;
}
else {
_label = 5;
}
if (_label === 11) {
if (( !( P>B ) )) {
_label = 12;
}
else {
_label = 7;
break $L4;
}
N = ( N-P );
if (( !( N===0 ) )) {
_label = 13;
}
else {
_label = 8;
break $L1;
}
if (( !( N<0 ) )) {
_label = 15;
break $L1;
}
else {
_label = 14;
}
N = ( N+P );
_label = 7;
break $L4;
}
else if (_label === 5) {
if (( P===N )) {
_label = 8;
break $L1;
}
else {
_label = 6;
}
_label = 7;
break $L4;
}
}
} while(0)
env.printLabel("ILLEGAL MOVE, REENTER IT ");
env.printAdvance3();
_label = 1;
continue $L1;
}
if (_label === 2) {
env.printLabel("I TOLD YOU NOT TO USE ZERO! COMPUTER WINS BY FORFEIT.");
env.printNewline();
W = 1;
return;
}
else if (_label === 8) {
if (( M===1 )) {
_label = 10;
}
else {
_label = 9;
}
if (_label === 10) {
env.printLabel("CONGRATULATIONS, YOU WIN.");
env.printNewline();
W = 1;
return;
}
else if (_label === 9) {
env.printLabel("TOUGH LUCK, YOU LOSE.");
env.printNewline();
W = 1;
return;
}
}
else if (_label === 15) {
W = 0;
return;
}
}
var main = function* () {
var _label;
var $x0;
var $x1;
$x0 = 0;
$x1 = 0;
env.printLabel("BATNUM");
env.printNewline();
env.printLabel("CREATIVE COMPUTING  MORRISTOWN, NEW JERSEY");
env.printNewline();
env.printNewline();
env.printNewline();
env.printNewline();
env.printLabel("THIS PROGRAM IS A 'BATTLE OF NUMBERS' GAME, WHERE THE");
env.printNewline();
env.printLabel("COMPUTER IS YOUR OPPONENT.");
env.printNewline();
env.printNewline();
env.printLabel("THE GAME STARTS WITH AN ASSUMED PILE OF OBJECTS. YOU");
env.printNewline();
env.printLabel("AND YOUR OPPONENT ALTERNATELY REMOVE OBJECTS FROM THE PILE.");
env.printNewline();
env.printLabel("WINNING IS DEFINED IN ADVANCE AS TAKING THE LAST OBJECT OR");
env.printNewline();
env.printLabel("NOT. YOU CAN ALSO SPECIFY SOME OTHER BEGINNING CONDITIONS.");
env.printNewline();
env.printLabel("DON'T USE ZERO, HOWEVER, IN PLAYING THE GAME.");
env.printNewline();
env.printLabel("ENTER A NEGATIVE NUMBER FOR NEW PILE SIZE TO STOP PLAYING.");
env.printNewline();
env.printNewline();
$L1: while (1) {
env.printLabel("ENTER PILE SIZE");
env.printNewline();
env.printNewline();
N = yield env.input();
if (( !( N<1 ) )) {
_label = 3;
}
else {
_label = 2;
}
if (_label === 2) {
_label = 1;
continue $L1;
}
else if (_label === 3) {
if (( !( N===Math.trunc( N ) ) )) {
_label = 17;
}
else {
_label = 4;
}
$L5: do {
if (_label === 4) {
if (( N<1 )) {
_label = 17;
break $L5;
}
else {
_label = 5;
}
$L7: while (1) {
env.printLabel("ENTER WIN OPTION - 1 TO TAKE LAST, 2 TO AVOID LAST: ");
env.printNewline();
env.printNewline();
M = yield env.input();
if (( M===1 )) {
_label = 7;
break $L7;
}
else {
_label = 6;
}
if (( !( M===2 ) )) {
_label = 5;
continue $L7;
}
else {
_label = 7;
break $L7;
}
}
$L10: while (1) {
env.printLabel("ENTER MIN AND MAX ");
env.printNewline();
env.printNewline();
A = yield env.input();
env.printNewline();
B = yield env.input();
if (( A>B )) {
_label = 7;
continue $L10;
}
else {
_label = 8;
}
if (( A<1 )) {
_label = 7;
continue $L10;
}
else {
_label = 9;
}
if (( !( A===Math.trunc( A ) ) )) {
_label = 7;
continue $L10;
}
else {
_label = 10;
}
if (( !( B===Math.trunc( B ) ) )) {
_label = 7;
continue $L10;
}
else {
_label = 11;
break $L10;
}
}
$L15: while (1) {
env.printLabel("ENTER START OPTION - 1 COMPUTER FIRST, 2 YOU FIRST ");
env.printNewline();
env.printNewline();
$x1 = yield env.input();
env.printNewline();
env.printNewline();
if (( $x1===1 )) {
_label = 13;
break $L15;
}
else {
_label = 12;
}
if (( !( $x1===2 ) )) {
_label = 11;
continue $L15;
}
else {
_label = 13;
break $L15;
}
}
C = ( A+B );
if (( $x1===2 )) {
_label = 15;
}
else {
_label = 14;
}
$L19: while (1) {
if (_label === 14) {
yield* fn$0();
if (( W===1 )) {
_label = 17;
break $L5;
}
else {
_label = 15;
continue $L19;
}
}
else if (_label === 15) {
yield* fn$1();
if (( W===1 )) {
_label = 17;
break $L5;
}
else {
_label = 16;
}
_label = 14;
continue $L19;
}
}
}
} while(0)
$x0 = 1;
$L25: while (1) {
if (( ( $x0-10 )>0 )) {
_label = 1;
continue $L1;
}
else {
_label = 19;
}
env.printNewline();
$x0 = ( $x0+1 );
_label = 18;
continue $L25;
}
}
}
}
env.run(main());