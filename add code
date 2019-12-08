module renderer.font;
import api.structs.vector2;
import std.typecons;
import std.conv;

/** Represents a font */
class Font {
	/** Simple glyphs */
	Glyph[dchar] glyphs;

	/** A list of all the ligatures and the strings they represent */
	Glyph[dstring] ligatures;

	/** Amount of space to add in between a pair of given dchars */
	double[dchar[2]] kerning;

	/** How large is the em height in this font */
	double emHeight;

	/** How far up is the baseline in this font */
	double baselineHeight;
}

alias Curve = Vector2[3];

/** Represents a single glyph */
class Glyph {
	/** How wide is this glyph */
	double width;

	/** The segments of this glyph */
	Curve[] segments;
}

package final class TruetypeReader {

	struct Table {
		uint checksum;
		uint offset;
		uint length;
	}

	struct HeadTable {
		double versionNumber;
		double fontRevision;
		uint checksumAdjustment;
		uint magicNumber;
		ushort flags;
		ushort unitsPerEm;
		double created;
		double modified;
		short xMin;
		short yMin;
		short xMax;
		short yMax;
		ushort macStyle;
		ushort lowestRecPPEM;
		short fontDirectionHint;
		short indexToLocFormat;
		short glyphDataFormat;
	}

	enum TrueGlyphType {
		Simple,
		Compound,
	}

	struct TrueGlyph {
		short numberOfContours;
		short xMin;
		short yMin;
		short xMax;
		short yMax;

		TrueGlyphType type;
		ushort[] contourEnds;
		TruePoint[] points;
	}

	struct TruePoint {
		bool onCurve;
		double x;
		double y;
	}

	immutable(ubyte)[] data;
	size_t pos;

	uint scalarType;
	ushort searchRange;
	ushort entrySelector;
	ushort rangeShift;

	Table[string] tables;

	HeadTable head;

	ushort glyphCount;

	size_t[dchar] cmap;

	Glyph[dchar] glyphMap;

	this(immutable(ubyte)[] data) {
		this.data = data;

		readOffsetTables();
		readHeadTable();
		getGlyphCount();
		readGlyphMap();
	}

	T get(T)() {
		ubyte[T.sizeof] slice;
		foreach (i; 0..T.sizeof) slice[i] = data[pos + i];
		for (size_t i = 0; i < T.sizeof / 2; ++i) {
			size_t j = T.sizeof - i - 1;
			slice[i] ^= slice[j];
			slice[j] ^= slice[i];
			slice[i] ^= slice[j];
		}
		T res = *cast(T*)slice.ptr;
		pos += T.sizeof;
		return res;
	}

	short getFword() {
		return get!short();
	}

	double getFixed() {
		return get!int() / cast(double)(1 << 16);
	}

	string getString(size_t length) {
		char[] result = new char[length];
		foreach (i; 0..length) {
			result[i] = get!char();
		}
		return result.idup;
	}

	double getDate() {
		get!ulong();
		return 0;
	}

	void readOffsetTables() {
		scalarType = get!uint();
		const numTables = get!ushort();
		searchRange = get!ushort();
		entrySelector = get!ushort();
		rangeShift = get!ushort();

		foreach (i; 0..numTables) {
			string tag = getString(4);

			Table table;
			table.checksum = get!uint();
			table.offset = get!uint();
			table.length = get!uint();

			tables[tag] = table;

			if (tag != "head") {
				assert(calculateTableChecksum(table.offset, table.length) == table.checksum);
			}
		}
	}

	uint calculateTableChecksum(uint offset, uint length) {
		const old = pos;
		pos = offset;

		uint sum = 0;
		foreach (i; 0..(length + 3) / 4) {
			sum += get!uint();
		}

		pos = old;
		return sum;
	}

	void readHeadTable() {
		assert("head" in tables);
		pos = tables["head"].offset;

		head.versionNumber = getFixed();
		head.fontRevision = getFixed();
		head.checksumAdjustment = get!uint();
		head.magicNumber = get!uint();
		assert(head.magicNumber == 0x5f0f3cf5);
		head.flags = get!ushort();
		head.unitsPerEm = get!ushort();
		head.created = getDate();
		head.modified = getDate();
		head.xMin = getFword();
		head.yMin = getFword();
		head.xMax = getFword();
		head.yMax = getFword();
		head.macStyle = get!ushort();
		head.lowestRecPPEM = get!ushort();
		head.fontDirectionHint = get!short();
		head.indexToLocFormat = get!short();
		head.glyphDataFormat = get!short();
	}

	void readGlyphMap() {
		assert("cmap" in tables);
		pos = tables["cmap"].offset;

		const start = pos;

		const ushort cmapVersion = get!ushort(); // version (0)
		assert(cmapVersion == 0);

		const ushort numSubtables = get!ushort();

		ulong offset = -1;
		for (auto i = numSubtables - 1; i >= 0; --i) {
			pos = start + 4 + i * 8;
			const platformID = get!ushort();
			const encodingID = get!ushort();
			const tableOffset = get!uint();

			if ((platformID == 3 && (encodingID == 0 || encodingID == 1 || encodingID == 10)) ||
				(platformID == 0 && (encodingID == 0 || encodingID == 1 || encodingID == 2
					|| encodingID == 3 || encodingID == 4))) {
				offset = tableOffset;
				break;
			}
		}

		assert(offset != -1);

		pos = start + offset;
		const format = get!ushort();

		if (format == 12) {
			get!ushort(); // skip reserved
			get!uint(); // length
			get!uint(); // language

			const groupCount = get!uint();

			foreach (i; 0..groupCount) {
				const startCharCode = get!uint();
				const endCharCode = get!uint();
				auto glyphId = get!uint();

				foreach (c; startCharCode..endCharCode + 1) {
					cmap[cast(dchar)c] = glyphId;
					glyphId++;
				}
			}
		}
		else if (format == 4) {
			get!ushort(); // length
			get!ushort(); // language

			const segCount = get!ushort() / 2;

			foreach (i; 0..3) get!ushort();

			foreach (i; 0..segCount - 1) {
				pos = start + offset + 14 + (2 * i);
				const endCount = get!ushort();
				pos = start + offset + 16 + segCount * 2 + (2 * i);
				const startCount = get!ushort();
				pos = start + offset + 16 + segCount * 4 + (2 * i);
				const idDelta = get!short();
				const glyphIndexOffset = start + offset + 16 + segCount * 6 + (2 * i);
				pos = glyphIndexOffset;
				const idRangeOffset = get!ushort();
				foreach (c; startCount..endCount + 1) {
					ushort glyphIndex;
					if (idRangeOffset != 0) {
						pos = glyphIndexOffset + idRangeOffset + (c - startCount) * 2;
						glyphIndex = get!ushort();
						if (glyphIndex != 0) {
							glyphIndex += idDelta;
						}
					}
					else {
						glyphIndex = cast(ushort)(c + idDelta);
					}
					cmap[cast(dchar)c] = glyphIndex;
				}
			}
		}
		else assert(0);
	}

	void getGlyphCount() {
		assert("maxp" in tables);
		const old = pos;
		pos = tables["maxp"].offset + 4;
		glyphCount = get!ushort();
		pos = old;
	}

	uint getGlyphOffset(size_t index) {
		assert("loca" in tables);
		const old = pos;
		const table = tables["loca"];
		uint offset;
		if (head.indexToLocFormat == 1) {
			pos = table.offset + index * 4;
			offset = get!uint();
		}
		else {
			pos = table.offset + index * 2;
			offset = get!ushort() * 2;
		}
		pos = old;
		return tables["glyf"].offset + offset;
	}

	Nullable!TrueGlyph readGlyph(size_t index) {
		const offset = getGlyphOffset(index);
		const glyph = tables["glyf"];

		if (offset >= glyph.offset + glyph.length) {
			return Nullable!TrueGlyph();
		}

		assert(offset >= glyph.offset);
		assert(offset < glyph.offset + glyph.length);

		pos = offset;

		TrueGlyph res;

		res.numberOfContours = get!short();
		res.xMin = getFword();
		res.yMin = getFword();
		res.xMax = getFword();
		res.yMax = getFword();

		assert(res.numberOfContours >= -1);

		if (res.numberOfContours == -1) {
			// readCompoundGlyph(res);
			assert(0);
		}
		else {
			readSimpleGlyph(res);
		}

		return res.nullable!TrueGlyph;
	}

	void readSimpleGlyph(ref TrueGlyph res) {
		res.type = TrueGlyphType.Simple;
		res.contourEnds = [];

		ushort contourEndsMax = 0;

		res.points = [];

		foreach (i; 0..res.numberOfContours) {
			const end = get!ushort();
			res.contourEnds ~= end;
			if (end > contourEndsMax) {
				contourEndsMax = end;
			}
		}

		if (res.numberOfContours == 0) {
			return;
		}

		pos += get!ushort();

		const numPoints = contourEndsMax + 1;
		ubyte[] flags;

		for (ushort i = 0; i < numPoints; ++i) {
			const flag = get!ubyte();
			flags ~= flag;

			TruePoint point = {
				onCurve: (flag & 1) > 0, // ON_CURVE
				x: 0,
				y: 0
			};

			res.points ~= point;

			if ((flag & 8) > 0) { // REPEAT
				ubyte repeatCount = get!ubyte();
				assert(repeatCount > 0);
				i += repeatCount;
				while (repeatCount --> 0) {
					flags ~= flag;
					res.points ~= point;
				}
			}
		}

		double x = 0, y = 0;

		foreach (i; 0..numPoints) {
			const flag = flags[i];
			if ((flag & 2) > 0) { // X_IS_BYTE
				if ((flag & 16) > 0) { // X_DELTA
					x += cast(double)get!ubyte();
				}
				else {
					x -= cast(double)get!ubyte();
				}
			}
			else if ((~cast(int)flag & 16) > 0) { // X_DELTA
				x += cast(double)get!short();
			}

			res.points[i].x = x;
		}

		foreach (i; 0..numPoints) {
			const flag = flags[i];
			if ((flag & 4) > 0) { // Y_IS_BYTE
				if ((flag & 32) > 0) { // Y_DELTA
					y += cast(double)get!ubyte();
				}
				else {
					y -= cast(double)get!ubyte();
				}
			}
			else if ((~cast(int)flag & 32) > 0) { // Y_DELTA
				y += cast(double)get!short();
			}

			res.points[i].y = y;
		}
	}

	Glyph getGlyph(dchar c) {
		if (c in glyphMap) {
			return glyphMap[c];
		}
		else {
			size_t index;
			if (c in cmap) index = cmap[c];
			return glyphMap[c] = getGlyph(index);
		}
	}

	Glyph getGlyph(size_t index) {
		Nullable!TrueGlyph maybeGlyph = readGlyph(index); // @suppress(dscanner.suspicious.unmodified)

		if (maybeGlyph.isNull) {
			return null;
		}

		Glyph res = new Glyph;
		TrueGlyph glyph = maybeGlyph.get;

		const scale = 1.0 / head.unitsPerEm;

		res.width = (cast(int)glyph.xMax - cast(int)glyph.xMin) * scale;

		const translate = Vector2(-cast(int)head.xMin, -cast(int)head.yMin);

		Vector2 transform(Vector2 vec) {
			vec = vec + translate;
			return Vector2(vec.x * scale, vec.y * -scale) + Vector2(0, 1);
		}

		TruePoint[][] contours = [[]];
		size_t c = 0;
		bool prevOnCurve = true;
		foreach (p; 0..glyph.points.length) {
			const point = glyph.points[p];

			if (!point.onCurve) {
				if (prevOnCurve) {
					prevOnCurve = false;
				}
				else {
					TruePoint interPoint = {
						onCurve: true,
						x: (glyph.points[p - 1].x + point.x) / 2,
						y: (glyph.points[p - 1].y + point.y) / 2,
					};
					contours[$ - 1] ~= interPoint;
				}
			}
			else {
				prevOnCurve = true;
			}

			contours[$ - 1] ~= point;

			if (p == glyph.contourEnds[c]) {
				contours ~= [[]];
			}
		}

		foreach (contour; contours) {
			foreach (p; 0..contour.length) {
				const point = contour[p];

				if (!point.onCurve) {
					if (res.segments != []) {
						assert(res.segments[$ - 1] != res.segments[$ - 1]);
						res.segments[$ - 1][1] = transform(Vector2(point.x, point.y));
					}
				}
				else {
					bool must = true;
					auto nextP = p;

					while (must || !contour[nextP].onCurve) {
						must = false;
						nextP = (nextP + 1) % contour.length;
					}
					const next = contour[nextP];
					res.segments ~= [
						transform(Vector2(point.x, point.y)),
						Vector2(),
						transform(Vector2(next.x, next.y)),
					];
				}
			}
		}

		return res;
	}

}

/** Convert the quadratic Bezier curves that comprise the glyph into lines */
Glyph linearize(Glyph glyph, double precision = 0.1) {
	Glyph res = new Glyph;
	res.width = glyph.width;

	foreach (seg; glyph.segments) {
		if (seg[1] == seg[1]) { // check for NaN
			Vector2 prev = seg[0];
			for (double step = 0; step < 1; step += precision) {
				const a = seg[0].lerp(seg[1], step);
				const b = seg[1].lerp(seg[2], step);
				const p = a.lerp(b, step);
				res.segments ~= [prev, Vector2(), p];
				prev = p;
			}
			if ((prev - seg[2]).magnitude > 1e-6) {
				res.segments ~= [prev, Vector2(), seg[2]];
			}
			// res.segments ~= [seg[0], Vector2(), seg[1]];
			// res.segments ~= [seg[1], Vector2(), seg[2]];
		}
		else {
			res.segments ~= seg;
		}
	}

	return res;
}
