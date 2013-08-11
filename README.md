# CodeBattle Server

*   [codebattle-server][20]
*   [codebattle-proto][21]
*   [codebattle-ai][22]
*   [codebattle-client][23]


CodeBattle designs for AI Battle. Like Google AI Challenge, Or Python VS Ruby.

You write your own AI (any language), connect to server, and battle with other AI.

There is an official server running at:
##### IP:     106.186.21.40
##### obPort: 11011
##### aiPort: 11012

#### What is obPort and aiPort?

The [client][1] as observer, to display the battle scene.

AI cannot createroom.
createroom action are done by client.

![clientstart][2]

In client start interface, Ip is the server Ip, Port is the obPort.
**obPort** just used for the GUI Client.

Then client create room, if everything goes well, you will see the **roomid** at the top of the battle scene.

Your AI will using this roomid to identify which room will join in.
Two AI join the same room, then they can battle each other.

AI join in a room, send command to Server. So there should be a socket connection between AI and the server.

**aiPort** is used for AI connect to the server.


#### NOTE

you can check out the code to you local computer, and modify code, test functions, or do anything else.

And I will be happy if you send pull request to improve the Server.


#### Deploy

1. git clone & cd project folder
2. git submodule init
3. git submodule foreach git pull
4. copy submodule/proto/*.proto src/
5. rebar get-deps
6. rebar compile
7. modify server.config if necessary
8. ./start.sh


#### How to write AI

AI & Server communicate via [google protobuf][3] .

This project's proto files and details see [here][4].

Also, there are some [Explain & AI examples][5].

#### Client

Codebattle client is an Unity3d project.
You can found it [here][1]. checkout it, and compile by yourself.

Or, Download the [Windows Executable Files][6].



[1]: https://github.com/yueyoum/codebattle-client
[2]: http://i1297.photobucket.com/albums/ag23/yueyoum/clientstart_zps807722a8.png
[3]: https://developers.google.com/protocol-buffers/docs/overview
[4]: https://github.com/yueyoum/codebattle-proto
[5]: https://github.com/yueyoum/codebattle-ai
[6]: http://pan.baidu.com/share/link?shareid=2250845780&uk=3942742758
[20]: https://github.com/yueyoum/codebattle-server
[21]: https://github.com/yueyoum/codebattle-proto
[22]: https://github.com/yueyoum/codebattle-ai
[23]: https://github.com/yueyoum/codebattle-client