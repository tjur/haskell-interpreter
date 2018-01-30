import "./prismjs/prism.js";
import "./codeflask/src/codeflask.js"

import { SocketManager } from "./sockets";
import { flask } from "./flask";

const socketManager = new SocketManager();
let sourceCode = '';

flask.onUpdate(code => sourceCode = code);

document.onkeydown = (event: KeyboardEvent) => {
	if (event.keyCode === 116) {
		event.preventDefault();
		event.stopPropagation();
		socketManager.sendCode(sourceCode);
	}
};
