//register_doit();

local_get(["sync", IP, Port]);
variable_get(["channel_keys"], function(x) {register_doit(x)});


function register_doit(x)
{
    if (typeof x === 'undefined')
    {
	    setTimeout(function() {variable_get(["channel_keys"], function(x) {register_doit(x)});}, 1000);
    }

    else if ( ( x.length === 1 ) && ( x.pop() === -6 ) )
    {
	    variable_get(["id"], new_channel);
    }

    else
    {
	    console.log("did not work, x was");
	    console.log(x);
    }
}

function new_channel(id)
{
    if (id === -1)
    {
        variable_get(["id"], new_channel);
    }
    else
    {
	    variable_get(["balance"], function(x) {new_channel2(id,x);})
    }
}


function new_channel2(id, bal)
{
    C = Math.min(Math.floor(bal/2), 1000000);

    local_get(["new_channel", IP, Port, C, Math.floor(C/1.1), 50]);
    // id unused!
}
