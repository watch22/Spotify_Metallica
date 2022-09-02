#Metallica
-- 1. Filter everything down to studio albums
SELECT DISTINCT
								A.album_id,
								A.album_name, 
								A.album_release_year,
								A.track_id
FROM song_table as A
-- FILTER OUT LIVE ALBUMS
LEFT JOIN
				(SELECT album_id,
								AVG (liveness) as live
					FROM song_table
                    GROUP BY album_id
                    HAVING live > 0.5) as B
                    ON A.album_id <> B.album_id
WHERE artist_name = 'Metallica'
#remove deluxe and special editions
AND album_name NOT LIKE  '%Deluxe%'
AND album_name NOT LIKE '%Special%'
AND album_name NOT LIKE '%Live%'
# remove albums released after latest studio album
AND album_release_year <= 2016
#remove remaining releases
AND album_name NOT IN ('Metallica Through The Never (Music From The Motion Picture)', 'Lulu',  'Some Kind Of Monster', 'Garage Inc.', 'S&M' ,'…And Justice for All (Remastered)')
ORDER BY album_release_year;

-- 2. Create table with track_ids (using oprevious query) to join to main table
CREATE TABLE metallica_tracks AS 
	SELECT DISTINCT
								A.album_id,
								A.album_name, 
								A.album_release_year,
								A.track_id
FROM song_table as A
-- FILTER OUT LIVE ALBUMS
LEFT JOIN
				(SELECT album_id,
								AVG (liveness) as live
					FROM song_table
                    GROUP BY album_id
                    HAVING live > 0.5) as B
                    ON A.album_id <> B.album_id
WHERE artist_name = 'Metallica'
#remove deluxe and special editions
AND album_name NOT LIKE  '%Deluxe%'
AND album_name NOT LIKE '%Special%'
AND album_name NOT LIKE '%Live%'
# remove albums released after latest studio album
AND album_release_year <= 2016
#remove remaining releases
AND album_name NOT IN ('Metallica Through The Never (Music From The Motion Picture)', 'Lulu',  'Some Kind Of Monster', 'Garage Inc.', 'S&M' ,'…And Justice for All (Remastered)')
ORDER BY album_release_year;
    
-- 3. JOIN metallica_tracks with song_table and select necessary columns & Create table "metallica_studio"
CREATE TABLE metallica_studio AS 
SELECT a.artist_name, 
			a.album_name, 
            a.track_number,
            a.track_name,
            a.speechiness,
            a.instrumentalness,
            a.acousticness,
            a.valence,
			a.energy,
            a.danceability,
			a.loudness,
            a.tempo,
            a.time_signature,
            a.duration_ms/60000 as duration_min,
            a.key_mode,
            a.track_id,
            a.album_release_year
FROM 
			song_table as a
RIGHT JOIN 
			metallica_tracks as b
ON  
			a.track_id = b.track_id
ORDER BY album_release_year,
					track_number;

-- Check if table was created properly
Select *
From metallica_studio;

-- 4. Update table to remove track and album titles with "Remastered"
UPDATE metallica_studio
SET album_name = REPLACE(album_name,'(Remastered)', ''),
		track_name = REPLACE(track_name,' - Remastered', '');
        
SELECT * FROM metallica_studio;
        
-- 5. Make sure Death Magnetic is still obscenely loud/compressed
SELECT album_name, round(avg(loudness * energy),3) as dB
FROM metallica_studio
GROUP BY album_name
ORDER BY dB DESC;
-- yup, all good

-- 6. Headbang_index
-- 6a. danceability is inverse headbangability?
SELECT album_name, 
				round(avg(danceability),3) as hb
FROM metallica_studio
GROUP BY album_name
ORDER BY hb; #Kill 'Em All - Death Magnetic - Ride the Lightning

-- Relate to tempo
SELECT album_name, 
				round(avg(tempo),0) as speed
FROM metallica_studio
GROUP BY album_name
ORDER BY speed;

#AVERAGE TEMPO (Studio albums): 126 bpm

SELECT album_name, 
				round(avg(danceability) * (avg(tempo)/126),3) as hb
FROM metallica_studio
GROUP BY album_name
ORDER BY hb;

# Introduce energy/dynamic range
SELECT album_name, 
				round(1 / (avg(danceability) * avg(energy) * (avg(tempo)/126)),3) as hb_index
FROM metallica_studio
GROUP BY album_name
ORDER BY hb_index DESC;

SELECT track_name,
                round(1/(danceability * energy * tempo/126),2) as hb_index
FROM metallica_studio
ORDER BY hb_index;


-- 7. Album Length?
SELECT album_name, sum(duration_min) 
FROM metallica_studio
GROUP BY album_name
ORDER BY sum(duration_min) DESC;









SELECT track_name, instrumentalness
FROM metallica_studio
ORDER BY instrumentalness DESC
LIMIT 10;
