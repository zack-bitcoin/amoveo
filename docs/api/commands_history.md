Looking up from history
============

The "address_history" tool is for seaching through a time period of blocks, and finding out who sent money to an address, and who received money from that address.
It prints messages like
```
gave 115510407 to BO/jDBPc0bpu3rfv4vahJpcvzUgLRtCenILu+nSiUkmvdiWUAEJ/GqEmipYULhEdEv+2jses7zU3Y/CyErjhJ5I= at 1090
gave 114782856 to BKV+hn/9S6QLckdSOpiS/CCQQ75Je+uHNAAWxkvCoHhL8Vv7eq17mQhT3PO1bTtIwgzYWWyAErZ8UUcAwJBGnaI= at 1089
gave 126380966 to BK8i+BBDW93etv1aZosHJDkxj8J5hWsJNTSusjHWkDXim9DnNxDEqtxpByqDHwMvghAiGEo7yomEj+ZoSUNJfF0= at 1088
gave 511031647 to BEUPZZqVe77E7Z1AAVAMLeqKJ1F1Dpu3pp09GaB7kdEtvkjGZu+ksOOJ1tXFtpdlap5dhr+ic889rwqE1Y+4pQ8= at 1087
gave 252007971 to BEUPZZqVe77E7Z1AAVAMLeqKJ1F1Dpu3pp09GaB7kdEtvkjGZu+ksOOJ1tXFtpdlap5dhr+ic889rwqE1Y+4pQ8= at 1086
received 70000000 from BGDXGmovuTEr3CFfzY0xr0zXS3purkIPPZsljZv4gvQe87K6W8be1IpwDwmqhgcSIlhTvb9Q7pgrl+h2dmXGEXM= at 1059
received 70000000 from BGDXGmovuTEr3CFfzY0xr0zXS3purkIPPZsljZv4gvQe87K6W8be1IpwDwmqhgcSIlhTvb9Q7pgrl+h2dmXGEXM= at 1058
received 70000000 from BGDXGmovuTEr3CFfzY0xr0zXS3purkIPPZsljZv4gvQe87K6W8be1IpwDwmqhgcSIlhTvb9Q7pgrl+h2dmXGEXM= at 1057
received 70000000 from BGDXGmovuTEr3CFfzY0xr0zXS3purkIPPZsljZv4gvQe87K6W8be1IpwDwmqhgcSIlhTvb9Q7pgrl+h2dmXGEXM= at 1055
received 70000000 from BGDXGmovuTEr3CFfzY0xr0zXS3purkIPPZsljZv4gvQe87K6W8be1IpwDwmqhgcSIlhTvb9Q7pgrl+h2dmXGEXM= at 1054
received 70000000 from BGDXGmovuTEr3CFfzY0xr0zXS3purkIPPZsljZv4gvQe87K6W8be1IpwDwmqhgcSIlhTvb9Q7pgrl+h2dmXGEXM= at 1052
gave 119690318 to BAJjbMF6rajiZN/cwPs3jDaWwt1rgpu6w5GZ3CpC6kgPdY47DSCU6u3zM96Z9E6BaoWf+osLBCY4MF/WXFbYsOo= at 938
```
and it returns a list of 2-tuples. the first element is the block height of the tx, the second element is the tx. like this:
```
[{1090,
  {create_acc_tx,<<4,172,139,168,43,197,230,251,123,40,24,
                   143,44,135,123,198,85,186,224,22,255,185,
                   95,162,...>>,
                 192,152050,
                 <<4,239,227,12,19,220,209,186,110,222,183,239,226,246,161,
                   38,151,47,205,72,11,...>>,
                 115510407}},
 {1089,
  {spend,<<4,172,139,168,43,197,230,251,123,40,24,143,44,
           135,123,198,85,186,224,22,255,185,...>>,
         190,152050,
         <<4,165,126,134,127,253,75,164,11,114,71,82,58,152,146, ... >>, 239393939}}]
```

To search through the blocks between (Height - Many) and height, use this:
`amoveo_utils:address_history(Pubkey, Many, Height).`

Height defaults as the current highest block.
`amoveo_utils:address_history(Pubkey, Many).`

Many defaults as 200
`amoveo_utils:address_history(Pubkey).`