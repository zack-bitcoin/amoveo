
macro Draw int 1 int 0 int 0 return ;

: or_die not if Draw else then ;

macro reveal ( Reveal Commit -- bool )
  swap dup tuck hash == or_die call drop drop ;
% If a secret is improperly revealed, the contract defaults to case 0. a draw.
  

% macro Commit1 int 1 hash ;
% macro Commit2 int 2 hash ;

macro Win1 int 0 Amount ; 
macro Win2 int 1 Amount ; 

macro player1revealed Commit1 reveal drop int 2 Win1 ;
macro player2revealed Commit2 reveal drop int 2 Win2 ;
: bothRevealed Commit2 reveal swap
          Commit1 reveal bxor int 2 rem
	  int 3 swap
	  if Win1 else Win2 then ;

%syntax for case statements.
macro -> == if drop drop ;
macro -- return else then drop ;

macro main
  int 1 -> player1revealed -- 
  int 2 -> player2revealed --
  int 3 -> bothRevealed call --
  drop Draw ;

main