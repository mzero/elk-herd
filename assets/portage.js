// This is the JavaScript side of Portage.elm

var hookup_ports = function(app, report_url) {

  var subscribe = function(port, func) {
    var p = app.ports[port];
    if (p) {
      p.subscribe(func);
    } else {
      console.log('port ' + port + ' is missing, skipping');
    }
  };

  var send = function(port, value) {
    var p = app.ports[port];
    if (p) {
      p.send(value);
    } else {
      console.log('port ' + port + ' is missing, cannot send');
    }
  }


  subscribe("readBinaryFile", function (file) {
    let name = file.name;

    try {
      let reader = new FileReader();

      reader.onload = function() {
        let bytes = new Uint8Array(reader.result);
        send("binaryFileContents_", Array.from(bytes));
      };

      reader.onerror = function(err) {
        console.log('failed to read binary file', name, err);
        send("binaryFileError", err.toString());
      };

      reader.readAsArrayBuffer(file);
    }
    catch (err) {
      reportError(err)
    }
  });

  subscribe("writeBinaryFile_", function(e) {
    let fileName = e[0];
    let mimeType = e[1];
    let byteArrays = e[2];

    let asBuffer = function(byteArray) {
      let buffer = new ArrayBuffer(byteArray.length);
      let bytes = new Uint8Array(buffer);
      bytes.set(byteArray, 0);
      return buffer;
    };

    let contents = byteArrays.map(asBuffer);

    let blob = new Blob(contents, {type: mimeType});
    let url = window.URL.createObjectURL(blob);
    let elem = window.document.createElement('a');
    elem.href = url
    elem.download = fileName;
    elem.click();
    window.URL.revokeObjectURL(url);
  });


  subscribe("readAudioFile", function(e) {
    let stereoSupported = e[0];
    let file = e[1];
    let name = file.name;

    let reportError = function(err) {
      console.log('failed to read audio file', name, err);
      send("sampleDataError", err.toString());
    };

    try {
      let reader = new FileReader();
      reader.onload = function() {
        let ctx = new OfflineAudioContext(1, 48000, 48000);

        ctx.decodeAudioData(reader.result).then(function(buffer){

          let srcStereo = buffer.numberOfChannels == 2;
          let dstStereo = srcStereo && stereoSupported;

          let nSamples = buffer.length;

          let floatSamples0 = buffer.getChannelData(0);
          let floatSamples1 = null;
          if (dstStereo) {
            floatSamples1 = buffer.getChannelData(1);
          }

          let frameSize = dstStereo ? 4 : 2;
          let sampleBytes = nSamples * frameSize;

          let fileBuffer = new ArrayBuffer(64 + sampleBytes);

          let header = new DataView(fileBuffer, 0, 64);
          header.setUint8(  0, 0);                // type
          header.setUint8(  1, dstStereo ? 1 : 0);// stereo
          header.setUint32( 4, sampleBytes);      // sample data length in bytes
          header.setUint32( 8, 48000);            // sample rate
          header.setUint32(12, 0);                // first sample index
          header.setUint32(16, nSamples-1);       // last sample index
          header.setUint8( 20, 0x7f);             // loop type (= no loop)

          let samples = new DataView(fileBuffer, 64, sampleBytes);
          for (let iv of floatSamples0.entries()) {
            let i = iv[0]
            let v = Math.floor(iv[1] * 32767.5);

            samples.setInt16(i*frameSize, v);
          }
          if (dstStereo) {
            for (let iv of floatSamples1.entries()) {
              let i = iv[0]
              let v = Math.floor(iv[1] * 32767.5);

              samples.setInt16(i*frameSize+2, v);
            }
          }

          let chunks = [];
          const chunkSize = 0x2000;
          let fileBytes = new Uint8Array(fileBuffer);
          for (let r = 0; r < fileBytes.length; r += chunkSize) {
            chunks.push(Array.from(fileBytes.slice(r, r+chunkSize)));
          }

          send("sampleData", chunks);
        },
        reportError
        );
      };

      reader.onerror = reportError;
      reader.readAsArrayBuffer(file);
    }
    catch (err) {
      reportError(err)
    }
  });

  subscribe("writeAudioFile_", function(nb) {
    let name = nb[0];
    let bss = nb[1];

    let nBytes = 0;
    for (let bs of bss) {
      nBytes += bs.length;
    }

    let fileBuffer = new ArrayBuffer(nBytes);
    let fileBytes = new Uint8Array(fileBuffer);
    let offset = 0;
    for (let bs of bss) {
      fileBytes.set(bs, offset);
      offset += bs.length;
    }

    let header = new DataView(fileBuffer, 0, 64);
    const sampleType        = header.getUint8(  0);
    const sampleStereo      = header.getUint8(  1) == 1;
    const sampleBytes       = header.getUint32( 4);
    const sampleRate        = header.getUint32( 8);
    const firstSampleIndex  = header.getUint32(12);
    const lastSampleIndex   = header.getUint32(16);
    const loopType          = header.getUint8( 20);

    // console.log("read samples for", name);
    // console.log("sampleType", sampleType);
    // console.log("sampleStereo", sampleStereo);
    // console.log("sampleBytes", sampleBytes);
    // console.log("sampleRate", sampleRate);
    // console.log("firstSampleIndex", firstSampleIndex);
    // console.log("lastSampleIndex", lastSampleIndex);
    // console.log("loopType", loopType);

    const dataChunkLen = sampleBytes;
    const fmtChunkLen = 16;
    const riffChunkLen = 4 + (8 + fmtChunkLen) + (8 + dataChunkLen);
    const wavFileLen = 8 + riffChunkLen;
    const nChannels = sampleStereo ? 2 : 1;

    let wavHeader = new DataView(fileBuffer, 20, 44);
    let setTag = function(o, tag) {
      wavHeader.setUint8(o+0, tag.charCodeAt(0));
      wavHeader.setUint8(o+1, tag.charCodeAt(1));
      wavHeader.setUint8(o+2, tag.charCodeAt(2));
      wavHeader.setUint8(o+3, tag.charCodeAt(3));
    };

              setTag(    0, 'RIFF');
    wavHeader.setUint32( 4, riffChunkLen,     true);
              setTag(    8, 'WAVE');
              setTag(   12, 'fmt ');
    wavHeader.setUint32(16, fmtChunkLen,      true);
    wavHeader.setUint16(20, 1,                true);  // format PCM
    wavHeader.setUint16(22, nChannels,        true);  // num channels
    wavHeader.setUint32(24, sampleRate,       true);
    wavHeader.setUint32(28, 2 * sampleRate * nChannels,
                                              true);  // byte rate
    wavHeader.setUint16(32, 2,                true);  // block align
    wavHeader.setUint16(34, 16,               true);  // bits per sample
              setTag(   36, 'data');
    wavHeader.setUint32(40, dataChunkLen,     true);

    let sampleData = new DataView(fileBuffer, 64, sampleBytes);
    for (let offset = 0; offset < sampleBytes; offset += 2) {
      sampleData.setInt16(offset, sampleData.getInt16(offset), true);
    }

    let wavBlob = new Blob([wavHeader, sampleData], {type : 'audio/wav'});
    let wavUrl = URL.createObjectURL(wavBlob);
    let downloader = document.createElement('a');
    downloader.href = wavUrl;
    downloader.download = name;
    downloader.click();
    URL.revokeObjectURL(wavUrl);
  });

  $('body').on('dragenter', false);
  $('body').on('dragover', false);
  $('body').on('dragleave', false);
  $('body').on('drop', false);
  $('body').on('drop', '.drop-items', function(ev) {
    let processEntry = function(path, entry) {
      let path_ = path.concat([entry.name]);
      if (entry.isFile) {
        entry.file(function(file) {
          send("droppedFile", [path_, file]);
        });
        // N.B.: This will fail here if index.html is loaded from a file
      }
      if (entry.isDirectory) {
        entry.createReader().readEntries(function(subEntries) {
          for (const subEntry of subEntries) {
            processEntry(path_, subEntry);
          }
        });
      }
    };
    let items = ev.originalEvent.dataTransfer.items;

    for (let i = 0; i < items.length; i++) {
      let entry = items[i].webkitGetAsEntry();
      if (entry) {
        processEntry([], entry);
      }
    }

    return false;
  });


  var midiAccess;
  var selectedMidiInPort;
  var selectedMidiOutPort;
  var midiInKey = "__midi-in__";
  var midiOutKey = "__midi-out__";

  var closeMidiAccess;

  var genMidiError = function(ePair) {
    if (closeMidiAccess) {
      closeMidiAccess();
    }
    send("midiError", ePair);
  };
  var midiError = function(e) {
    genMidiError([e.name, e.message]);
  };



  var pubRecvMidi = function(e) {
    // console.log("recvMidi", e.data.length, e.data.slice(0, 12));
    send("recvMidi", Array.from(e.data));
  };

  var selectMidiPort = function(selectedPort, port) {
    if (selectedPort !== port) {
      if (selectedPort) {
        selectedPort.close();
        selectedPort = null;
      }
      if (port) {
        selectedPort = port;
        let req = selectedPort.open();
        req.then(function(s){}, midiError);
      }
    }
    return selectedPort;
  };

  var selectMidiInPort = function(id) {
    localStorage.setItem(midiInKey, id);
    if (midiAccess) {
      selectedMidiInPort = selectMidiPort(selectedMidiInPort,
                                          midiAccess.inputs.get(id));
      if (selectedMidiInPort) {
        selectedMidiInPort.onmidimessage = pubRecvMidi;
      }
    }
  }

  var selectMidiOutPort = function(id) {
    localStorage.setItem(midiOutKey, id)
    if (midiAccess) {
      selectedMidiOutPort = selectMidiPort(selectedMidiOutPort,
                                           midiAccess.outputs.get(id));
    }
  }

  var pubMidiAccess = function() {
    let cvt = function(p) {
      return {
        'id': p.id,
        'manufacturer': p.manufacturer,
        'name': p.name,
        'state': p.state,
        'connection': p.connection
      };
    };

    let process = function(ports, key, selectFn)  {
      let list = [];
      let allClosed = true;
      let selection = localStorage.getItem(key);
      for (let p of ports.values()) {
        list.push(cvt(p));
        allClosed = allClosed && p.connection == "closed";
      }
      if (allClosed && selection !== null)  {
        selectFn(selection);
      }
      return list;
    };

    let inList = [];
    let outList = [];
    if (midiAccess) {
      inList = process(midiAccess.inputs, midiInKey, selectMidiInPort);
      outList = process(midiAccess.outputs, midiOutKey, selectMidiOutPort);
    }

    send("midiAccess", [inList, outList]);
  };

  var requestMidiAccess = function() {
    let setupMidiAccess = function(ma) {
      midiAccess = ma;
      midiAccess.onstatechange = pubMidiAccess;
      pubMidiAccess();
    };
    let rejected = function(e) {
      pubMidiAccess();
      midiError(e);
    }

    if (navigator.requestMIDIAccess) {
      let req = navigator.requestMIDIAccess({ 'sysex': true });
      req.then(setupMidiAccess, rejected);
    }
    else {
      let noWebMidi = function() {
        this.name = "NoWebMidi";
        this.message = "This browser doesn't support WebMIDI.";
      };
      let sendError = function() { midiError(new noWebMidi()); };
      setTimeout(sendError, 500);
    }
  };

  var closeMidiAccess = function() {
    selectMidiPort(selectedMidiInPort, null);
    selectMidiPort(selectedMidiOutPort, null);
    if (midiAccess) {
      midiAccess.onstatechange = null;
    }
    midiAccess = null;
  }

  subscribe("genMidiError", genMidiError);
  subscribe("selectMidiIn", selectMidiInPort);
  subscribe("selectMidiOut", selectMidiOutPort);

  var sendMidi = function(data) {
    if (selectedMidiOutPort) {
      // console.log("sendMidi", data.length, data.slice(0, 12));
      try {
        selectedMidiOutPort.send(data);
      }
      catch(e) {
        midiError(e);
      }
    }
  };
  subscribe("sendMidi_", sendMidi);
  subscribe("sendMidiElectronApi_", function(data8) {
    let len8 = data8.length;
    let len7 = len8 + Math.ceil(len8/7);
    let buf = new Uint8Array(6 + len7 + 1);

    buf.set([0xf0, 0x00, 0x20, 0x3c, 0x10, 0x00], 0); // Elektron SysEx header

    let w = 6;
    for (let r = 0; r < len8; r += 7) {
      let s = data8.slice(r, r+7);
      let hi = s.reduce((a, b, i) => a | ((b & 0x80) >> (i+1)), 0);

      buf.set([hi], w);
      buf.set(s.map( x => x & 0x7f ), w+1);
      w += 8;
    }

    buf.set([0xf7], 6+len7);                          // SysEx end

    sendMidi(buf);
  });
  subscribe("closeMidi", closeMidiAccess);


  var appVersionKey = "__app-version__"
  var appOptInKey = "__app-optin__"
  var appInstanceKey = "__app-instance__"

  var newUUIDv4 = function() {
    let bs = crypto.getRandomValues(new Uint8Array(32));
    bs[12] = 4;
    bs[16] = bs[16] & 0x3 | 0x8;

    let s = ((i,n) =>
      Array.from(bs.slice(i, i+n).values())
           .map(b => (b & 0xf).toString(16)).join(''));

    return [s(0,8),s(8,4),s(12,4),s(16,4),s(20,12)].join('-');
  }

  var appInstance = localStorage.getItem(appInstanceKey);
  if (appInstance == null) {
    appInstance = newUUIDv4();
    localStorage.setItem(appInstanceKey, appInstance);
  }
  var report = function(key, value) {
    let optIn = localStorage.getItem(appOptInKey);
    if (report_url && optIn === "true") {
      $.ajax({
        method: "POST",
        url: report_url,
        contentType: 'application/json; charset=UTF-8',
        dataType: 'json',
        data: JSON.stringify({ id: appInstance, event: key, args: value })
      }).fail(function(){
        console.log("reporting(", appInstance, ", ", key, ", ", value, ")")
      });
    }
  };
  var reportUA = function() { report('user-agent', navigator.userAgent); };

  subscribe("startUpApp", function(e) {
    // Tooltips:
    //    Enable dynamically as they can be added later
    //    Add a mouseleave handler (also dynamically) as tooltips get "stuck"
    //    on with Elm, if an event fires on a button, Also, add same on click
    //    since if the operation disables the button, no mouseleave will
    //    be sent by the browser!
    $('body').tooltip({selector:'[data-toggle="tooltip"]'});
    $('body').on("mouseleave", '[data-toggle="tooltip"]', function(ev) {
      $(this).tooltip('hide');
    });
    $('body').on("click", '[data-toggle="tooltip"]', function(ev) {
      $(this).tooltip('hide');
    });

    // This duplicates the logic in Windows1252.digitaktClean that is applied
    // to pattern and sound names. But doing it in the Events.onInput handler
    // means that if the text changes, the cursor jumps to th end. Doing it here
    // on keydown preserves the cursor position.

    let digitaktFrom  = "abcdefghijklmnopqrstuvwxyzäåæçñöøü";
    let digitaktTo    = "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÅÆÇÑÖØÜ";
    let digitaktLegal = " !#$%&()+-0123456789=@ABCDEFGHIJKLMNOPQRSTUVWXYZ^_~ÄÅÆÇÑÖØÜß";

    $('body').on("keydown", '.digitakt-charset', function(ev) {
      var c = ev.key;
      if (ev.ctrlKey || ev.metaKey || c.length > 1 || ev.originalEvent.repeat) {
        return true;
      }

      let i = digitaktFrom.indexOf(c);
      if (i >= 0) {
        // Character should be replaced, and there's no good way to do this as
        // an event, so just insert it directly.
        c = digitaktTo[i];
        let start = this.selectionStart;
        let end = this.selectionEnd;
        this.value = this.value.slice(0, start) + c + this.value.slice(end);
        this.selectionStart = this.selectionEnd = start + c.length;

        this.dispatchEvent(new InputEvent('input'));

        return false;

      }

      // Supress the event if it isn't a legal character
      return digitaktLegal.indexOf(c) >= 0;
    });

    requestMidiAccess();
    reportUA();

    let maybeParse = function(j) { return j ? JSON.parse(j) : null; };
    let getSetting = function(k, t, d) {
      v = maybeParse(localStorage.getItem(k));
      return (v !== null && typeof v == t) ? v : d;
    };

    let ver = getSetting(appVersionKey, "number", 0);
    ver = ver < 1 ? Math.floor(ver * 100) : ver;

    let optIn = getSetting(appOptInKey, "boolean", null);

    send("storedAppVersionAndOptIn", [ver, optIn]);
  });

  var onlyInstanceChannel;
  if (window.BroadcastChannel) {
    onlyInstanceChannel = new BroadcastChannel('only_instance');
    onlyInstanceChannel.postMessage('starting');
    onlyInstanceChannel.onmessage = function(ev) {
      closeMidiAccess();
      send("onlyInstanceMinder", null);
    };
  }

  subscribe("resetAppVersion", function(v) {
    localStorage.clear();
    localStorage.setItem(appVersionKey, JSON.stringify(v));
    localStorage.setItem(appInstanceKey, appInstance);
  });
  subscribe("setAppVersion", function(v) {
    localStorage.setItem(appVersionKey, JSON.stringify(v));
  });
  subscribe("setOptIn", function(b) {
    localStorage.setItem(appOptInKey, JSON.stringify(b));
    reportUA();
  });
  subscribe("report", function(kv) {
    report(kv[0], kv[1]);
  });
  subscribe("log",  console.log);
};
