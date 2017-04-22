/*
 * -------------------------------------------------------------------
 * @author Zack-Bitcoin
 * @copyright (C) 2017, <Aeternity>
 * @link "https://aeternity.com"
 *
 * @coauthor Zwilla
 * @copyright (C) 2017, <Zwilla Research>
 * @link "https://www.the-internet-of-money.de/aeternity"
 *
 * @doc
 *
 * According to our dual licensing model, this program can be used either
 * under the terms of the GNU Affero General Public License, version 3,
 * or under a proprietary license.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * @end
 * -------------------------------------------------------------------
 */

var pubkey;
pubkey = document.createElement("div");
pubkey.id  = "pubkey";

document.body.appendChild(pubkey);


function pubkey_func()
{
    variable_get(["pubkey"], pubkey1);
}

setTimeout(pubkey_func, 2000);


function pubkey1(id)
{
    console.log("pubkey 1");

    var balance       = document.getElementById("pubkey");
    balance.innerHTML = "pubkey ".concat(atob(id));

    console.log("pubkey 3");
}
