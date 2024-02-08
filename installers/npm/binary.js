var fs = require('fs');
var package = require('./package.json');
var path = require('path');



// MAIN
//
// This function is used by install.js and by the bin/zokka backup that gets
// called when --ignore-scripts is enabled.


module.exports = function()
{
	// figure out package of binary
  // this originally had a regex to calculate a new version by stripping
  // alpha/beta monikers, but I actually want to keep the subpackages in
  // lockstep with the main package because ultimately I envision the release
  // process being automatic
	var version = package.version;
  var subPackageName = findSubPackage(process.platform, process.arch);

	verifyPlatform(version, subPackageName);

	var fileName = process.platform === 'win32' ? 'zokka.exe' : 'zokka';

	try
	{
		var subBinaryPath = require.resolve(subPackageName + '/' + fileName);
	}
	catch (error)
	{
		if (error && error.code === 'MODULE_NOT_FOUND')
		{
			exitFailure(version, missingSubPackageHelp(subPackageName));
		}
		else
		{
			exitFailure(version, 'I had trouble requiring the binary package for your platform (' + subPackageName + '):\n\n' + error);
		}
	}

	// Yarn 2 and later ("Berry") always invokes `node` (regardless of configuration)
	// so we cannot do any optimizations there
	var isYarnBerry = /\byarn\/(?!1\.)/.test(process.env.npm_config_user_agent || "");

	// as mentioned in bin/zokka we cannot do any optimizations on Windows
	if (process.platform === 'win32' || isYarnBerry)
	{
		return subBinaryPath;
	}

	// figure out where to put the binary
	var binaryPath = path.resolve(__dirname, package.bin.zokka);
	var tmpPath = binaryPath + '.tmp';

	// optimize by replacing the JS bin/zokka with the native binary directly
	try
	{
		// atomically replace the file with a hard link to the binary
		fs.linkSync(subBinaryPath, tmpPath);
		fs.renameSync(tmpPath, binaryPath);
	}
	catch (error)
	{
		exitFailure(version, 'I had some trouble writing file to disk. It is saying:\n\n' + error);
	}

	return binaryPath;
}



// VERIFY PLATFORM


function verifyPlatform(version, subPackageName)
{
	if (subPackageName in package.optionalDependencies) return;

	var situation = process.platform + '_' + process.arch;
	console.error(
		'-- ERROR -----------------------------------------------------------------------\n\n'
		+ 'I am detecting that your computer (' + situation + ') may not be compatible with any\n'
		+ 'of the official pre-built binaries.\n\n'
		+ 'I recommend against using the npm installer for your situation. Check out the\n'
		+ 'alternative installers at https://github.com/zokka-dev/compiler/releases/tag/' + version + '\n'
		+ 'to see if there is something that will work better for you.\n\n'
		+ 'From there I recommend asking for guidance on Slack or Discourse to find someone\n'
		+ 'who can help with your specific situation.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
	process.exit(1);
}



// EXIT FAILURE


function exitFailure(version, message)
{
	console.error(
		'-- ERROR -----------------------------------------------------------------------\n\n'
		+ message
		+ '\n\nNOTE: You can avoid npm entirely by downloading directly from:\n'
		+ 'https://github.com/zokka-dev/compiler/releases/tag/' + version + '\n'
		+ 'All this package does is distribute a file from there.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
	process.exit(1);
}



// MISSING SUB PACKAGE HELP


function missingSubPackageHelp(subPackageName)
{
	return (
		'I tried to get `zokka` from ' + subPackageName + ', but something went wrong.\n'
		+ 'This can happen if you use the "--omit=optional" or "--no-optional" npm flag, or\n'
		+ 'if your "node_modules" folder was copied over from a different computer (or VM).\n'
	);
}



// VERIFY
// This goes away after we fix https://github.com/Zokka-Dev/zokka-compiler/issues/11
// In particular our need to return the x64 version on darwin instead of arm64


function findSubPackage(platform, arch)
{
  var packagePrefix = '@zokka/zokka-binary-';
  return packagePrefix + process.platform + '_' + process.arch;
}
